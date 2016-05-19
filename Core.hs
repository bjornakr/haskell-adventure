module Core where 
    import Entity
    import qualified Data.Map as Map
    import qualified Data.Set as Set
    import Data.Hashable (Hashable)
    import CoreTypes
    
    data ActionResult = ActionResult String GameState | ConversationTrigger String GameState
    data Observation = Observation Id String


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
        where f x = Map.insert key x stateMap

    removeState' :: String -> String -> StateMap -> StateMap
    removeState' key val stateMap =
        case Map.lookup key stateMap of
            (Just ss) -> Map.insert key (Set.delete val ss) stateMap
            Nothing -> stateMap

    addState :: String -> String -> GameState -> GameState
    addState key val state@GameState { stateMap = stateMap } =
        state { stateMap = addState' key val stateMap }

    removeState :: String -> String -> GameState -> GameState
    removeState key val state@GameState { stateMap = stateMap } =
        state { stateMap = removeState' key val stateMap }

    hasState :: String -> String -> GameState -> Bool
    hasState key val = hasState' key val . stateMap

    addActor :: World -> Room -> Actor -> World
    addActor rooms r a = updateEntity (addActorToRoom r a) rooms

    addActorToRoom :: Room -> Actor -> Room
    addActorToRoom (Room rid rs is as) actor = Room rid rs is (addOrUpdateEntity actor as)

    addItemToRoom :: Room -> Item -> Room
    addItemToRoom (Room rId rs items as) item = Room rId rs (addOrUpdateEntity item items) as

    removeItemFromRoom :: Item -> Room -> Room
    removeItemFromRoom item (Room rId rs items as) = Room rId rs (removeEntity item items) as

    findItemInRoom :: Room -> Id -> Maybe Item
    findItemInRoom (Room _ _ items _) id0 = findEntityById id0 items

    updateItemInRoom :: Item -> Room -> Room
    updateItemInRoom item (Room id0 exits items actors) =
        Room id0 exits (updateEntity item items) actors

    addItemToInventory :: Item -> GameState -> GameState
    addItemToInventory item state@GameState { player = (Player roomId inventory) } =
        state { player = Player roomId (item:inventory) }

    removeItemFromInventory :: Item -> GameState -> GameState
    removeItemFromInventory item state@GameState { player = (Player roomId inventory) } =
        state { player = Player roomId (removeEntity item inventory) }
    
    movePlayerToRoomId :: RoomId -> GameState -> GameState
    movePlayerToRoomId newRoomId state@GameState { player = (Player _ inventory) } =
        state { player = Player newRoomId inventory }

    removeItemFromWorld :: Item -> GameState -> GameState
    removeItemFromWorld item state@GameState { world = world } =
        state { world = map (removeItemFromRoom item) world }

    destroyItem :: Item -> GameState -> GameState
    destroyItem item = removeItemFromInventory item . removeItemFromWorld item

    transferItemFromWorldToPlayer :: Item -> GameState -> GameState
    transferItemFromWorldToPlayer item = removeItemFromWorld item . addItemToInventory item

    updateItem :: Item -> GameState -> GameState
    updateItem updatedItem state@GameState { player = (Player roomId inventory), world = world } =
        state { 
            player = Player roomId (updateEntity updatedItem inventory),
            world = map (updateItemInRoom updatedItem) world
        }

    addExit :: Maybe Room -> Maybe Room -> GameState -> GameState
    addExit Nothing _ gamestate = gamestate
    addExit _ Nothing gamestate = gamestate
    addExit (Just (Room roomId exits items actors)) (Just room2) state@GameState { world = world } =
        state { world = updateEntity (Room roomId (getId room2 : exits) items actors) world }

    linkRooms' :: Maybe Room -> Maybe Room -> GameState -> GameState
    linkRooms' room1 room2 = addExit room2 room1 . addExit room1 room2

    linkRooms :: Id -> Id -> GameState -> GameState
    linkRooms id1 id2 state@GameState { world = world } =
        linkRooms' (findEntityById id1 world) (findEntityById id2 world) state

    toObservation :: (Show a, Entity a) => a -> Observation
    toObservation a = Observation (getId a) (show a)


    findObservationById :: [Observation] -> Id -> Maybe String
    findObservationById [] _ = Nothing
    findObservationById (Observation id0 s : xs) id1
        | id0 == id1 = Just s
        | otherwise = findObservationById xs id1

    updateItemDescription :: Item -> String -> GameState -> GameState
    updateItemDescription (LooseItem (ItemDetails id desc)) newDesc =
        updateItem (LooseItem (ItemDetails id newDesc))
    updateItemDescription (StaticItem (ItemDetails id desc)) newDesc =
        updateItem (StaticItem (ItemDetails id newDesc))

    movePlayerToRoom :: Room -> GameState -> GameState
    movePlayerToRoom newRoom state@GameState { player = Player _ inventory } =
        state { player = Player (getId newRoom) inventory }
