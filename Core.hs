module Core where 
    import Entity
    import qualified Data.HashMap.Strict as Map
    import qualified Data.Set as Set
    import Data.Hashable (Hashable)
    
    type RoomId = Id
    type Inventory = [Item]
    type Description = String


    data Actor = Actor Id [Item] Description deriving (Eq)
    instance Entity Actor where
        getId (Actor id _ _) = id
    instance Show Actor where
        show (Actor _ _ desc) = desc


    data Item = LooseItem ItemDetails | StaticItem ItemDetails deriving (Eq)
    getItemDetails :: Item -> ItemDetails
    getItemDetails (LooseItem itemDetails) = itemDetails
    getItemDetails (StaticItem itemDetails) = itemDetails

    instance Show Item where
        show item = show (getItemDetails item)

    instance Entity Item where
        getId item = getId (getItemDetails item)    

    data ItemDetails = ItemDetails Id Description deriving (Eq)
    instance Show ItemDetails where
        show (ItemDetails _ desc) = desc
    instance Entity ItemDetails where
        getId (ItemDetails id _) = id

    data Room = Room Id [RoomId] [Item] [Actor] deriving (Eq)
    instance Entity Room where
        getId (Room id _ _ _) = id

    instance Show Room where
        show (Room id exitIds items actors) = 
            "You are in " ++ id ++ ".\n\n" ++
            "You see " ++ (show (map getId items)) ++ ".\n\n" ++
            --"There is someone here " ++ (show (map getId actors)) ++ ".\n\n" ++
            "Exits to " ++ (show exitIds) ++ ".\n\n"

    data Player = Player RoomId Inventory deriving (Show, Eq)
    type World = [Room]
    type StateMap a = Map.HashMap a (Set.Set String)
    data GameState = GameState Player World (StateMap String)
    instance Show GameState where
        show (GameState (Player roomId inventory) rooms stateMap) =
            case (findEntityById roomId rooms) of
                Nothing -> error $ "Missing room: " ++ roomId
                (Just room) ->
                    (show room) ++
                    "You have " ++ (show inventory) ++ ".\n\n" ++
                    --"States: " ++ (show stateMap) ++ ".\n\n" ++
                    "What would you like to do?\n" ++
                    "[W]alk to | [L]ook (at) | [P]ick up | Co[M]bine\n" ++
                    "[G]ive    | [T]alk to   | Pu[S]h    | Pu[L]l   \n" ++
                    "[O]pen    | [C]lose     | [U]se     |          \n"

    data ActionResult = ActionResult GameState String
    data Observation = Observation Id String







    hasState :: (Eq a, Hashable a) => StateMap a -> a -> String -> Bool
    hasState stateMap key s =
        case Map.lookup key stateMap of
             (Just ss) -> Set.member s ss
             Nothing -> False

    addState' :: (Eq a, Hashable a) => StateMap a -> a -> String -> StateMap a
    addState' stateMap key s =
        case Map.lookup key stateMap of        
            (Just ss) -> f (Set.insert s ss)
            Nothing -> f (Set.singleton s)
        where f x = (Map.insert key x stateMap)

    removeState' :: (Eq a, Hashable a) => StateMap a -> a -> String -> StateMap a
    removeState' stateMap key val =
        case Map.lookup key stateMap of
            (Just ss) -> Map.insert key (Set.delete val ss) stateMap
            Nothing -> stateMap


    addState :: GameState -> String -> String -> GameState
    addState (GameState player world stateMap) key val = GameState player world (addState' stateMap key val)

    removeState :: GameState -> String -> String -> GameState
    removeState (GameState player world stateMap) key val = GameState player world (removeState' stateMap key val)

    getGamestateFromActionResult :: ActionResult -> GameState
    getGamestateFromActionResult (ActionResult gamestate _) = gamestate

    getMessageFromActionResult :: ActionResult -> String
    getMessageFromActionResult (ActionResult _ message) = message

    addActor :: World -> Room -> Actor -> World
    addActor (rooms) r a = (updateEntity (addActorToRoom r a) rooms)

    addActorToRoom :: Room -> Actor -> Room
    addActorToRoom (Room rid rs is as) actor = Room rid rs is (addOrUpdateEntity actor as)

    addItemToRoom :: Room -> Item -> Room
    addItemToRoom (Room rId rs items as) item = Room rId rs (addOrUpdateEntity item items) as

    removeItemFromRooms :: [Room] -> Item -> [Room]
    removeItemFromRooms [] item = []
    removeItemFromRooms (room:rooms) item = (removeItemFromRoom room item):(removeItemFromRooms rooms item)

    removeItemFromRoom :: Room -> Item -> Room
    removeItemFromRoom (Room rId rs items as) item = (Room rId rs (removeEntity item items) as)

    changeItemsInRoom :: ([Item] -> [Item]) -> Room -> Room
    changeItemsInRoom f (Room roomId exits items actors) = Room roomId exits (f items) actors

    findItemInRoom :: Room -> Id -> Maybe Item
    findItemInRoom (Room _ _ items _) id = findEntityById id items

    updateItemInRoom :: Room -> Item -> Room
    updateItemInRoom (Room id exits items actors) item =
        Room id exits (updateEntity item items) actors

    addItemToInventory :: GameState -> Item -> GameState
    addItemToInventory (GameState (Player roomId inventory) world stateMap) item =
        (GameState (Player roomId (item:inventory)) world stateMap)

    setPlayerRoom :: GameState -> RoomId -> GameState
    setPlayerRoom (GameState (Player _ inventory) world stateMap) newRoomId =
        GameState (Player newRoomId inventory) world stateMap

    removeItemFromInventory :: GameState -> Item -> GameState
    removeItemFromInventory (GameState (Player roomId inventory) world stateMap) item =
        (GameState (Player roomId (removeEntity item inventory)) world stateMap)


    removeItemFromWorld :: GameState -> Item -> GameState
    removeItemFromWorld (GameState player world stateMap) item =
        GameState player (map (flip (removeItemFromRoom) item) world) stateMap

    destroyItem :: GameState -> Item -> GameState
    destroyItem gamestate item = removeItemFromInventory (removeItemFromWorld gamestate item) item

    transferItemFromWorldToPlayer :: GameState -> Item ->  GameState
    transferItemFromWorldToPlayer gamestate item =
        removeItemFromWorld (addItemToInventory gamestate item) item

    updateItem :: GameState -> Item -> GameState
    updateItem (GameState (Player roomId inventory) world stateMap) updatedItem =
        GameState 
            (Player roomId (updateEntity updatedItem inventory)) 
            (map (flip (updateItemInRoom) updatedItem) world)
            stateMap

    exchangeItem :: GameState -> Item -> Item -> GameState
    exchangeItem (GameState (Player roomId inventory) world stateMap) oldItem newItem =
        GameState 
            (Player roomId (exchangeEntity oldItem newItem inventory)) 
            (map (changeItemsInRoom (exchangeEntity oldItem newItem)) world)
            stateMap

    addExit :: GameState -> Maybe Room -> Maybe Room -> GameState
    addExit gamestate Nothing _ = gamestate
    addExit gamestate _ Nothing = gamestate
    addExit (GameState player world stateMap) (Just (Room roomId exits items actors)) (Just room2)  =
        GameState player (updateEntity (Room roomId ((getId room2):exits) items actors) world) stateMap

    linkRooms' :: GameState -> Maybe Room -> Maybe Room -> GameState
    linkRooms' gamestate Nothing _ = gamestate
    linkRooms' gamestate _ Nothing = gamestate
    linkRooms' gamestate room1 room2 = addExit (addExit gamestate room1 room2) room2 room1

    linkRooms :: GameState -> Id -> Id -> GameState
    linkRooms gamestate@(GameState _ world _) id1 id2 = linkRooms' gamestate (findEntityById id1 world) (findEntityById id2 world)

    toObservation :: (Show a, Entity a) => a -> Observation
    toObservation a = Observation (getId a) (show a)


    findObservationById :: [Observation] -> Id -> Maybe String
    findObservationById [] id = Nothing
    findObservationById ((Observation id' s):xs) id
        | id == id' = Just s
        | otherwise = findObservationById xs id

    updateItemDescription :: GameState -> Item -> String -> GameState
    updateItemDescription gamestate (LooseItem (ItemDetails id desc)) newDesc =
        updateItem gamestate (LooseItem (ItemDetails id newDesc))
    updateItemDescription gamestate (StaticItem (ItemDetails id desc)) newDesc =
        updateItem gamestate (StaticItem (ItemDetails id newDesc))
