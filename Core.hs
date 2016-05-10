module Core where 
    import Entity
    import qualified Data.Map as Map
    import qualified Data.Set as Set
    import Data.Hashable (Hashable)
    
    type RoomId = Id
    type Inventory = [Item]
    type Description = String


    data Actor = Actor Id [Item] Description deriving (Eq)
    instance Entity Actor where
        getId (Actor id0 _ _) = id0
    instance Show Actor where
        show (Actor _ _ desc) = desc


    data Item = LooseItem ItemDetails | StaticItem ItemDetails deriving (Eq)


    instance Show Item where
        show = show . getItemDetails

    instance Entity Item where
        getId = getId . getItemDetails

    data ItemDetails = ItemDetails Id Description deriving (Eq)
    
    instance Show ItemDetails where
        show (ItemDetails _ desc) = desc
    
    instance Entity ItemDetails where
        getId (ItemDetails id0 _) = id0

    data Room = Room Id [RoomId] [Item] [Actor] deriving (Eq)
    instance Entity Room where
        getId (Room id0 _ _ _) = id0

    instance Show Room where
        show (Room id exitIds items actors) = 
            "You are in " ++ id ++ ".\n\n" ++
            "You see " ++ (show (map getId items)) ++ ".\n\n" ++
            "There is someone here " ++ (show (map getId actors)) ++ ".\n\n" ++
            "Exits to " ++ (show exitIds) ++ ".\n\n"

    data Player = Player RoomId Inventory deriving (Show, Eq)
    
    type World = [Room]
    
    type StateMap = Map.Map String (Set.Set String) -- Enables several states per id.
    
    data GameState = GameState Player World StateMap
    
    data ActionResult = ActionResult String GameState | ConversationTrigger String GameState

    instance Show GameState where
        show (GameState (Player roomId inventory) rooms stateMap) =
            case (findEntityById roomId rooms) of
                Nothing -> error $ "Missing room: " ++ roomId
                (Just room) ->
                    (show room) ++
                    "You have " ++ (show (map getId inventory)) ++ "."
                    --"States: " ++ (show stateMap) ++ ".\n\n" ++


    data Observation = Observation Id String


    getItemDetails :: Item -> ItemDetails
    getItemDetails (LooseItem itemDetails) = itemDetails
    getItemDetails (StaticItem itemDetails) = itemDetails

    hasState' :: String -> String -> StateMap -> Bool
    hasState' key s stateMap =
        case Map.lookup key stateMap of
             (Just ss) -> Set.member s ss
             Nothing -> False

    addState' :: String -> String -> StateMap -> StateMap
    addState' key s stateMap =
        case Map.lookup key stateMap of        
            (Just ss) -> f (Set.insert s ss)
            Nothing -> f (Set.singleton s)
        where f x = (Map.insert key x stateMap)

    removeState' :: String -> String -> StateMap -> StateMap
    removeState' key val stateMap =
        case Map.lookup key stateMap of
            (Just ss) -> Map.insert key (Set.delete val ss) stateMap
            Nothing -> stateMap

    addState :: String -> String -> GameState -> GameState
    addState key val (GameState player world stateMap) = GameState player world (addState' key val stateMap)

    removeState :: String -> String -> GameState -> GameState
    removeState key val (GameState player world stateMap) = GameState player world (removeState' key val stateMap)

    hasState :: String -> String -> GameState -> Bool
    hasState key val (GameState player world stateMap) = hasState' key val stateMap

    addActor :: World -> Room -> Actor -> World
    addActor rooms r a = (updateEntity (addActorToRoom r a) rooms)

    addActorToRoom :: Room -> Actor -> Room
    addActorToRoom (Room rid rs is as) actor = Room rid rs is (addOrUpdateEntity actor as)

    addItemToRoom :: Room -> Item -> Room
    addItemToRoom (Room rId rs items as) item = Room rId rs (addOrUpdateEntity item items) as

    removeItemFromRooms :: [Room] -> Item -> [Room]
    removeItemFromRooms [] item = []
    removeItemFromRooms (room:rooms) item = (removeItemFromRoom room item):(removeItemFromRooms rooms item)

    removeItemFromRoom :: Room -> Item -> Room
    removeItemFromRoom (Room rId rs items as) item = (Room rId rs (removeEntity item items) as)

    --changeItemsInRoom :: ([Item] -> [Item]) -> Room -> Room
    --changeItemsInRoom f (Room roomId exits items actors) = Room roomId exits (f items) actors

    findItemInRoom :: Room -> Id -> Maybe Item
    findItemInRoom (Room _ _ items _) id0 = findEntityById id0 items

    updateItemInRoom :: Item -> Room -> Room
    updateItemInRoom item (Room id0 exits items actors) =
        Room id0 exits (updateEntity item items) actors

    addItemToInventory :: Item -> GameState -> GameState
    addItemToInventory item (GameState (Player roomId inventory) world stateMap) =
        (GameState (Player roomId (item:inventory)) world stateMap)

    movePlayerToRoomId :: RoomId -> GameState -> GameState
    movePlayerToRoomId newRoomId (GameState (Player _ inventory) world stateMap) =
        GameState (Player newRoomId inventory) world stateMap

    removeItemFromInventory :: Item -> GameState -> GameState
    removeItemFromInventory item (GameState (Player roomId inventory) world stateMap) =
        (GameState (Player roomId (removeEntity item inventory)) world stateMap)


    removeItemFromWorld :: Item -> GameState -> GameState
    removeItemFromWorld item (GameState player world stateMap) =
        GameState player (map (flip (removeItemFromRoom) item) world) stateMap

    destroyItem :: Item -> GameState -> GameState
    destroyItem item = removeItemFromInventory item . removeItemFromWorld item

    transferItemFromWorldToPlayer :: Item -> GameState -> GameState
    transferItemFromWorldToPlayer item = removeItemFromWorld item . addItemToInventory item

    updateItem :: Item -> GameState -> GameState
    updateItem updatedItem (GameState (Player roomId inventory) world stateMap) =
        GameState 
            (Player roomId (updateEntity updatedItem inventory)) 
            (map (updateItemInRoom updatedItem) world)
            stateMap

    --exchangeItem :: GameState -> Item -> Item -> GameState
    --exchangeItem (GameState (Player roomId inventory) world stateMap) oldItem newItem =
    --    GameState 
    --        (Player roomId (exchangeEntity oldItem newItem inventory)) 
    --        (map (changeItemsInRoom (exchangeEntity oldItem newItem)) world)
    --        stateMap

    addExit :: Maybe Room -> Maybe Room -> GameState -> GameState
    addExit Nothing _ gamestate = gamestate
    addExit _ Nothing gamestate = gamestate
    addExit (Just (Room roomId exits items actors)) (Just room2) (GameState player world stateMap) =
        GameState player (updateEntity (Room roomId ((getId room2):exits) items actors) world) stateMap

    linkRooms' :: Maybe Room -> Maybe Room -> GameState -> GameState
    linkRooms' room1 room2 = addExit room2 room1 . addExit room1 room2

    linkRooms :: Id -> Id -> GameState -> GameState
    linkRooms id1 id2 gamestate@(GameState _ world _) =
        linkRooms' (findEntityById id1 world) (findEntityById id2 world) gamestate

    toObservation :: (Show a, Entity a) => a -> Observation
    toObservation a = Observation (getId a) (show a)


    findObservationById :: [Observation] -> Id -> Maybe String
    findObservationById [] _ = Nothing
    findObservationById ((Observation id0 s):xs) id1
        | id0 == id1 = Just s
        | otherwise = findObservationById xs id1

    updateItemDescription :: Item -> String -> GameState -> GameState
    updateItemDescription (LooseItem (ItemDetails id desc)) newDesc =
        updateItem (LooseItem (ItemDetails id newDesc))
    updateItemDescription (StaticItem (ItemDetails id desc)) newDesc =
        updateItem (StaticItem (ItemDetails id newDesc))

    movePlayerToRoom :: Room -> GameState -> GameState
    movePlayerToRoom newRoom (GameState (Player _ inventory) world stateMap) =
        GameState (Player (getId newRoom) inventory) world stateMap
