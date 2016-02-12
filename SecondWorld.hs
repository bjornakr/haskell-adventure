import Data.List
import Data.List.Split

type Id = String
type Inventory = [Item]

class IdEq a where
    idEq :: a -> a -> Bool
    getId :: a -> Id
    idEq a1 a2 = getId a1 == getId a2

data Actor = Actor Id [Item] deriving (Show, Eq)
instance IdEq Actor where
    getId (Actor id _) = id

data Item = Item Id deriving (Show, Eq)
instance IdEq Item where
    getId (Item id) = id

data Room = Room Id [Room] [Item] [Actor] deriving (Show, Eq)
instance IdEq Room where
    getId (Room id _ _ _) = id

data World = World [Room] deriving (Show, Eq)
    
linkRooms :: World -> Room -> Room -> World
linkRooms w room1@(Room rid1 rs1 i1 a1) room2@(Room rid2 rs2 i2 a2) =
    addRooms w [Room rid1 (room2:rs1) i1 a1, Room rid2 (room1:rs2) i2 a2]

addRooms :: World -> [Room] -> World
addRooms w [] = w
addRooms w (r:rs) = addRooms (addRoom w r) rs

addRoom :: World -> Room -> World
addRoom (World rs) room = World (addOrUpdateEntity room rs)

addActor :: World -> Room -> Actor -> World
addActor (World rooms) r a =
    (World (updateEntity (addActorToRoom r a) rooms))

addActorToRoom :: Room -> Actor -> Room
addActorToRoom (Room rid rs is as) actor = Room rid rs is (addOrUpdateEntity actor as)

addItem :: World -> Room -> Item -> World
addItem (World rooms) room item = World (updateEntity (addItemToRoom room item) rooms)

addItemToRoom :: Room -> Item -> Room
addItemToRoom (Room rId rs items as) item = Room rId rs (addOrUpdateEntity item items) as

removeItemFromRoom :: Room -> Item -> Room
removeItemFromRoom (Room rId rs items as) item = (Room rId rs (removeEntity item items) as)

addItemToInventory :: Actor -> Item -> Actor
addItemToInventory (Actor id items) item = (Actor id (item:items))


transferItemFromRoomToActor :: World -> Item -> Room -> Actor -> World
transferItemFromRoomToActor world item room@(Room _ _ items actors) actor
    | roomHasItem && actorIsInRoom = addRoom world (removeItemFromRoom (addActorToRoom room (addItemToInventory actor item)) item)
    | otherwise = world
    where
        roomHasItem = findEntity item items == Just item
        actorIsInRoom = findEntity actor actors == Just actor

-- transferActorToAnotherRoom :: World -> Room -> Room -> Actor -> World
-- TODO: IMPLEMENT



-- ==== ENTITY FUNCTIONS ====
addOrUpdateEntity :: (IdEq a, Eq a) => a -> [a] -> [a]
addOrUpdateEntity e es
    | existingEntity == Nothing = e:es
    | otherwise = updateEntity e es
    where
        existingEntity = findEntity e es

findEntity :: IdEq a => a -> [a] -> Maybe a
findEntity e [] = Nothing
findEntity e' (e:es)
    | idEq e' e = Just e'
    | otherwise = findEntity e' es

findEntityById :: IdEq a => String -> [a] -> Maybe a
findEntityById id [] = Nothing
findEntityById id (e:es)
    | id == (getId e) = Just e
    | otherwise = findEntityById id es

updateEntity :: IdEq a => a -> [a] -> [a]
updateEntity ue [] = []
updateEntity ue (e:es)
    | idEq ue e = ue:es
    | otherwise = e:(updateEntity ue es)


removeEntity :: IdEq a => a -> [a] -> [a]
removeEntity e [] = []
removeEntity e' (e:es)
    | idEq e' e = es
    | otherwise = e:(removeEntity e' es)

-- ==== TESTS ====
item1 = Item "Item1"
actor1 = Actor "Actor1" []
room1 = Room "Room1" [] [] []
room2 = Room "Room2" [] [] []
room3 = Room "Room3" [] [] []
updatedRoom1 = Room "Room1" [room2] [] []
world = World [room1, room2]

transferItemFromRoomToActor_itemInSameRoomAsActor_itemIsTransfered =
    let room = Room "Room1" [] [item1] [actor1] in
    transferItemFromRoomToActor (World [room]) item1 room actor1 ==
        World [Room "Room1" [] [] [(Actor "Actor1" [item1])]]
transferItemFromRoomToActor_itemIsNotInRoom_nothingHappens =
    let room = Room "Room1" [] [] [actor1] in
    transferItemFromRoomToActor (World [room]) item1 room actor1 ==
        World [Room "Room1" [] [] [(Actor "Actor1" [])]]
transferItemFromRoomToActor_actorIsNotInRoom_nothingHappens =
    let room = Room "Room1" [] [item1] [] in
    transferItemFromRoomToActor (World [room]) item1 room actor1 ==
        World [Room "Room1" [] [item1] []]


addActorToRoom_noActorsInRoom_returnsRoomWithActor =
    addActorToRoom room1 actor1 == Room "Room1" [] [] [actor1]
addActorToRoom_actorAlreayInRoom_returnsRoomWithActorNotDuplicated =
    addActorToRoom (Room "Room1" [] [] [actor1]) actor1 == Room "Room1" [] [] [actor1]

linkRooms_twoUnlinkedRooms_roomsAreLinked =
    linkRooms world room1 room2 == World [Room "Room1" [room2] [] [], Room "Room2" [room1] [] []]
linkRooms_twoAlreadyLinkedRooms_nothingHappens =
    linkRooms (World [Room "Room1" [room2] [] [], Room "Room2" [room1] [] []]) room1 room2 == 
        World [Room "Room1" [room2] [] [], Room "Room2" [room1] [] []]
linkRooms_newRooms_roomsAreCreatedAndLinked =
    linkRooms (World []) room1 room2 == World [Room "Room2" [room1] [] [], Room "Room1" [room2] [] []]

addRoom_newRoom_roomIsAdded =
    addRoom world room3 == (World [room3, room1, room2])
addRoom_existingRoom_roomIsUpdated =
    addRoom world updatedRoom1 == (World [updatedRoom1, room2])

findEntity_noEntities_returnsNothing = findEntity room1 [] == Nothing
findEntity_entityExists_returnsEntity = findEntity room1 [room1] == Just room1
findEntity_entityDoesNotExist_returnsNothing = findEntity room3 [room1, room2] == Nothing
updateEntity_entityDoesNotExist_nothingHappens = updateEntity room1 [room2, room3] == [room2, room3]
updateEntity_entityExists_entityIsUpdated = updateEntity updatedRoom1 [room1, room2] == [updatedRoom1, room2]


testsAreOk =
    findEntity_noEntities_returnsNothing &&
    findEntity_entityExists_returnsEntity &&
    findEntity_entityDoesNotExist_returnsNothing &&
    updateEntity_entityDoesNotExist_nothingHappens &&
    updateEntity_entityExists_entityIsUpdated &&
    addRoom_newRoom_roomIsAdded &&
    addRoom_existingRoom_roomIsUpdated &&
    linkRooms_twoUnlinkedRooms_roomsAreLinked &&
    linkRooms_twoAlreadyLinkedRooms_nothingHappens &&
    linkRooms_newRooms_roomsAreCreatedAndLinked &&
    addActorToRoom_noActorsInRoom_returnsRoomWithActor &&
    addActorToRoom_actorAlreayInRoom_returnsRoomWithActorNotDuplicated







-- ==== UseCase / View ====

-- data Action = WalkTo Room | LookAt Item | PickUp Item | Use Item Item | Give Item Actor | TalkTo Actor
data Action = LookAt (Maybe Item) deriving (Show)

parseAction :: String -> Room -> Maybe Action
parseAction s (Room id exits items actors) = 
    let actionParts = splitOn " " s in
        case actionParts !! 0 of
            "L" -> Just (LookAt (findEntityById (actionParts !! 1) items))
            _ -> Nothing



parseObject :: String -> [Item] -> Maybe Item
parseObject s items = findEntityById s items


lookAt :: Maybe Item -> String
lookAt Nothing = "I can't look at that, señor."
lookAt (Just item) = "You study the " ++ (show item) ++ " in great detail."

respondAction :: Maybe Action -> String
respondAction Nothing = "I don't know how to do that, señor.\n"
respondAction (Just action) = respondValidAction action
    -- | action == LookAt (Maybe Item) = "Heey"
    -- | otherwise = "I don't know how to do that, señor.\n"

respondValidAction :: Action -> String
respondValidAction (LookAt item) = lookAt item

sampleRoom =
    Room 
        "Library"           -- ID
        []                  -- Exits
        [
            Item "Sword",
            Item "Key",
            Item "Book"
        ]                   -- Items
        [
            Actor "Player" [Item "Apple"]
        ]                   -- Actors

sampleWorld = World [sampleRoom]

description :: Room -> String
description (Room rId exits items _) =
    "You are in " ++ rId ++ ".\n\n" ++
    "You see " ++ (show items) ++ ".\n\n" ++
    "What would you like to do?\n" ++
    "[W]alk to | [L]ook at | [P]ick up | [U]se\n" ++
    "[G]ive    | [T]alk to | Pu[S]h    | Pu[L]l"

main :: IO ()
main = do
    putStrLn $ description sampleRoom
    action <- getLine
    putStrLn $ show $ respondAction $ parseAction action sampleRoom