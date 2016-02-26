import Data.List
import Data.List.Split

type Id = String
type Inventory = [Item]
type Description = String

class Entity a where
    idEq :: a -> a -> Bool
    getId :: a -> Id
    idEq a1 a2 = getId a1 == getId a2

class Observable a where
    getDescription :: a -> String

data Actor = Actor Id [Item] Description deriving (Show, Eq)
instance Entity Actor where
    getId (Actor id _ _) = id
instance Observable Actor where
    getDescription (Actor _ _ desc) = desc

data Player = Player Room Inventory deriving (Show, Eq)
instance Observable Player where
    getDescription (Player (Room roomId _ items _) inventory) = 
        "You are in " ++ roomId ++ ".\n\n" ++
        "You see " ++ (show items) ++ ".\n\n" ++
        "You have " ++ (show inventory) ++ ".\n\n" ++
        "What would you like to do?\n" ++
        "[W]alk to | [L]ook (at) | [P]ick up | [C]ombine\n" ++
        "[G]ive    | [T]alk to   | Pu[S]h    | Pu[L]l"

data Item = Item Id Description deriving (Eq)
instance Show Item where
    show (Item id _) = id
instance Entity Item where
    getId (Item id _) = id
instance Observable Item where
    getDescription (Item _ desc) = desc

data Room = Room Id [Room] [Item] [Actor] deriving (Show, Eq)
instance Entity Room where
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
addItemToInventory (Actor id items desc) item = (Actor id (item:items) desc)

--roomHasItem :: Room -> Item -> Bool
--roomHasItem (Room _ _ items _) item = findEntity item items == Just item

transferItemFromRoomToActor :: World -> Item -> Room -> Actor -> World
transferItemFromRoomToActor world item room@(Room _ _ items actors) actor
    | roomHasItem && actorIsInRoom = addRoom world (removeItemFromRoom (addActorToRoom room (addItemToInventory actor item)) item)
    | otherwise = world
    where
        roomHasItem = findEntity item items == Just item
        actorIsInRoom = findEntity actor actors == Just actor

-- transferActorToAnotherRoom :: World -> Room -> Room -> Actor -> World
-- TODO: IMPLEMENT

pickUpItem :: (Player, World) -> Item -> (Player, World)
pickUpItem ((Player room@(Room roomId exits items actors) inventory), world) item =
    (Player roomWithoutItem (item:inventory), addRoom world roomWithoutItem)
    where roomWithoutItem = removeItemFromRoom room item


-- ==== ENTITY FUNCTIONS ====
addOrUpdateEntity :: (Entity a, Eq a) => a -> [a] -> [a]
addOrUpdateEntity e es
    | existingEntity == Nothing = e:es
    | otherwise = updateEntity e es
    where
        existingEntity = findEntity e es

findEntity :: Entity a => a -> [a] -> Maybe a
findEntity e [] = Nothing
findEntity e' (e:es)
    | idEq e' e = Just e'
    | otherwise = findEntity e' es

findEntityById :: Entity a => String -> [a] -> Maybe a
findEntityById id [] = Nothing
findEntityById id (e:es)
    | id == (getId e) = Just e
    | otherwise = findEntityById id es

updateEntity :: Entity a => a -> [a] -> [a]
updateEntity ue [] = []
updateEntity ue (e:es)
    | idEq ue e = ue:es
    | otherwise = e:(updateEntity ue es)


removeEntity :: Entity a => a -> [a] -> [a]
removeEntity e [] = []
removeEntity e' (e:es)
    | idEq e' e = es
    | otherwise = e:(removeEntity e' es)

-- ==== TESTS ====
item1 = Item "Item1" "Test item 1"
actor1 = Actor "Actor1" [] "Test actor 1"
room1 = Room "Room1" [] [] []
room2 = Room "Room2" [] [] []
room3 = Room "Room3" [] [] []
updatedRoom1 = Room "Room1" [room2] [] []
world = World [room1, room2]

transferItemFromRoomToActor_itemInSameRoomAsActor_itemIsTransfered =
    let room = Room "Room1" [] [item1] [actor1] in
    transferItemFromRoomToActor (World [room]) item1 room actor1 ==
        World [Room "Room1" [] [] [(Actor "Actor1" [item1]) "Test actor 1"]]
transferItemFromRoomToActor_itemIsNotInRoom_nothingHappens =
    let room = Room "Room1" [] [] [actor1] in
    transferItemFromRoomToActor (World [room]) item1 room actor1 ==
        World [Room "Room1" [] [] [(Actor "Actor1" []) "Test actor 1"]]
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
--data Action = LookAt (Maybe Item) | LookAt (Maybe Actor) deriving (Show)

data Action = LookAt | LookAround | PickUp

parseAction :: String -> Maybe (Action, Id)
parseAction s = parseAction' (splitOn " " s)
    --let actionParts = splitOn " " s in
    --    case actionParts !! 0 of
    --        "L" -> Just (LookAt, actionParts !! 1)
    --        _ -> Nothing

parseAction' :: [String] -> Maybe (Action, Id)
parseAction' (verb:[])
    | verb == "L" = Just (LookAround, "NULL")
    | otherwise = Nothing
parseAction' (verb:id:ids) =
    case verb of
        "L" -> Just (LookAt, id)
        "P" -> Just (PickUp, id)
        _ -> Nothing



respondAction :: (Player, World) -> Maybe (Action, Id) -> ((Player, World), String)
respondAction gamestate Nothing = (gamestate, "I don't know how to do that, señor.\n")
respondAction gamestate (Just action) = respondValidAction gamestate action
--respondAction world@(World rooms) (Just action) = (world, snd $ respondValidAction (rooms !! 0) action)

respondValidAction :: (Player, World) -> (Action, Id) -> ((Player, World), String)
respondValidAction (player, world) (LookAround, _) = ((player, world), getDescription player)
respondValidAction gamestate (LookAt, id) = lookAtSomething gamestate id
respondValidAction gamestate@(Player (Room _ _ items _) _, w) (PickUp, id) =
    pickUpSomething gamestate (findEntityById id (items))

lookAtSomething :: (Player, World) -> Id -> ((Player, World), String)
lookAtSomething gamestate@(Player (Room _ _ items actors) inventory, w) id
--lookAtSomething room@(Room roomId exits items actors) id
    | item /= Nothing = (gamestate, lookAt item)
    | actor /= Nothing = (gamestate, lookAt actor)
    | otherwise = (gamestate, "You don't know where to look.")
    where
        item = findEntityById id (items ++ inventory)
        actor = findEntityById id actors

pickUpSomething :: (Player, World) -> Maybe Item -> ((Player, World), String)
pickUpSomething gamestate@(Player (Room _ _ items actors) inventory, w) (Just item@(Item itemId _)) =
    (pickUpItem gamestate item, "You pick up the " ++ itemId)
pickUpSomething gamestate Nothing =
    (gamestate, "How can you pick up that which does not exist?")


lookAt :: (Observable a) => Maybe a -> String
lookAt Nothing = "I can't look at that, señor."
lookAt (Just observable) = getDescription observable





sampleRoom =
    Room 
        "Library"           -- ID
        []                  -- Exits
        [
            Item "Sword" "A beautiful, shining, freshly polished sword.",
            Item "Key" "It is a golden key.",
            Item "Book" "The title is Zob Goblin and the Poggle of Buckletwig."
        ]                   -- Items
        [
            Actor "Player" [Item "Apple" "A red, shining apple."] "A fine, young lad."
        ]                   -- Actors

sampleWorld = World [sampleRoom]
samplePlayer = Player sampleRoom [Item "Gun" "It's a good, old Smith & Wesson."]


description :: Player -> String
description (Player (Room rId exits items _) inventory) =
    "You are in " ++ rId ++ ".\n\n" ++
    "You see " ++ (show items) ++ ".\n\n" ++
    "You have " ++ (show inventory) ++ ".\n\n" ++
    "What would you like to do?\n" ++
    "[W]alk to | [L]ook at | [P]ick up | [C]ombine\n" ++
    "[G]ive    | [T]alk to | Pu[S]h    | Pu[L]l"

gameLoop :: (Player, World) -> IO ()
gameLoop (player, world) = do
    --putStrLn $ description player
    action <- getLine
    let actionResponse = respondAction (player, world) (parseAction action)
    putStrLn $ snd actionResponse
    gameLoop (fst actionResponse)

main :: IO ()
main = do
    gameLoop (samplePlayer, sampleWorld)

    --putStrLn $ description player
    --action <- getLine
    --let a = respondAction (player, sampleWorld) (parseAction action)
    --putStrLn $ show $ snd $ a
    --putStrLn $ description (fst (fst a))
    --action <- getLine
    --putStrLn "Game over!"