import Data.List
import Data.List.Split

------------------------------------------------------------------------------------
-- TODO
--
-- Exits should be ids, not Rooms
-- DRY up the Maybe Rooms in the ActionResult
-- Some items should NOT be picked up
-- Open / Close
------------------------------------------------------------------------------------



------------------------------------------------------------------------------------
-- DEFINITIONS
------------------------------------------------------------------------------------

type Id = String
type RoomId = String
type Inventory = [Item]
type Description = String

class (Eq a) => Entity a where
    idEq :: a -> a -> Bool
    getId :: a -> Id
    getDescription :: a -> String
    toObservation :: a -> Observation
    idEq a1 a2 = getId a1 == getId a2
    toObservation a = Observation (getId a) (getDescription a)

data Actor = Actor Id [Item] Description deriving (Show, Eq)
instance Entity Actor where
    getId (Actor id _ _) = id
    getDescription (Actor _ _ desc) = desc


data Item = Item Id Description deriving (Eq)
instance Show Item where
    show (Item id _) = id
instance Entity Item where
    getId (Item id _) = id
    getDescription (Item _ desc) = desc

data Room = Room Id [Room] [Item] [Actor] deriving (Show, Eq)
instance Entity Room where
    getId (Room id _ _ _) = id
    getDescription (Room id exits items actors) = 
        "You are in " ++ id ++ ".\n\n" ++
        "You see " ++ (show items) ++ ".\n\n" ++
        "There is someone here " ++ (show (map getId actors)) ++ ".\n\n" ++
        "Exits to " ++ (show (map getId exits)) ++ ".\n\n"

data Player = Player RoomId Inventory deriving (Show, Eq)
type World = [Room]
data GameState = GameState Player World
instance Show GameState where
    show (GameState (Player roomId inventory) world) =
        (observeEntity roomId world) ++
        "You have " ++ (show inventory) ++ ".\n\n" ++
        "What would you like to do?\n" ++
        "[W]alk to | [L]ook (at) | [P]ick up | [C]ombine\n" ++
        "[G]ive    | [T]alk to   | Pu[S]h    | Pu[L]l"

data ActionResult = ActionResult GameState String
data Observation = Observation Id String






------------------------------------------------------------------------------------
-- ENTITY FUNCTIONS
------------------------------------------------------------------------------------
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

findEntityById :: Entity a => Id -> [a] -> Maybe a
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

exchangeEntity :: Entity a => a -> a -> [a] -> [a]
exchangeEntity _ _ [] = []
exchangeEntity oldEntity newEntity (e:es)
    | idEq oldEntity e = newEntity:es
    | otherwise = e:(exchangeEntity oldEntity newEntity es)


observeEntity' :: Entity a => Maybe a -> String
observeEntity' Nothing = ""
observeEntity' (Just e) = getDescription e

observeEntity :: Entity a => Id -> [a] -> String
observeEntity id entities = observeEntity' (findEntityById id entities)




------------------------------------------------------------------------------------
-- DOMAIN FUNCTIONS
------------------------------------------------------------------------------------

getGamestateFromActionResult :: ActionResult -> GameState
getGamestateFromActionResult (ActionResult gamestate _) = gamestate

getMessageFromActionResult :: ActionResult -> String
getMessageFromActionResult (ActionResult _ message) = message

linkRooms :: World -> Room -> Room -> World
linkRooms w room1@(Room rid1 rs1 i1 a1) room2@(Room rid2 rs2 i2 a2) =
    addRooms w [Room rid1 (room2:rs1) i1 a1, Room rid2 (room1:rs2) i2 a2]

addRooms :: World -> [Room] -> World
addRooms w [] = w
addRooms w (r:rs) = addRooms (addRoom w r) rs

addRoom :: World -> Room -> World
addRoom world room = addOrUpdateEntity room world

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
addItemToInventory (GameState (Player roomId inventory) world) item =
    (GameState (Player roomId (item:inventory)) world)

removeItemFromWorld :: GameState -> Item -> GameState
removeItemFromWorld (GameState player world) item = GameState player (map (flip (removeItemFromRoom) item) world )

transferItemFromWorldToPlayer :: GameState -> Item ->  GameState
transferItemFromWorldToPlayer gamestate@(GameState (Player roomId _) world) item =
    removeItemFromWorld (addItemToInventory gamestate item) item

updateItem :: GameState -> Item -> GameState
updateItem (GameState (Player roomId inventory) world) updatedItem =
    GameState (Player roomId (updateEntity updatedItem inventory)) (map (flip (updateItemInRoom) updatedItem) world)

exchangeItem :: GameState -> Item -> Item -> GameState
exchangeItem (GameState (Player roomId inventory) world) oldItem newItem =
    GameState 
        (Player roomId (exchangeEntity oldItem newItem inventory)) 
        (map (changeItemsInRoom (exchangeEntity oldItem newItem)) world)

findObservationById :: [Observation] -> Id -> Maybe String
findObservationById [] id = Nothing
findObservationById ((Observation id' s):xs) id
    | id == id' = Just s
    | otherwise = findObservationById xs id

combine :: GameState -> Item -> Item -> ActionResult
combine gamestate (Item "Toothbrush" _) (Item "Toilet" _) = 
    ActionResult 
        (updateItem (updateItem gamestate (Item "Toilet" "A super clean toilet")) (Item "Toothbrush" "The toothbrush looks rather unappealing."))
        "You give the toilet a good scrub."

combine gamestate i1@(Item "Toilet" _) i2@(Item "Toothbrush" _) = combine gamestate i2 i1

combine gamestate jaildoorKey@(Item "JaildoorKey" _) closedJaildoor@(Item "ClosedJaildoor" jaildoorDescription) = 
    ActionResult
        (exchangeItem (removeItemFromWorld gamestate jaildoorKey) closedJaildoor (Item "OpenJaildoor" jaildoorDescription))
        "You hear a click. Amazing, the key worked!"

combine gamestate i1@(Item "ClosedJaildoor" _) i2@(Item "JaildoorKey" _) = combine gamestate i2 i1

combine gamestate _ _ = ActionResult gamestate "You cannot combine those items."





------------------------------------------------------------------------------------
-- USE CASES / APPLICATION
------------------------------------------------------------------------------------

-- data Action = WalkTo Room | LookAt Item | PickUp Item | Use Item Item | Give Item Actor | TalkTo Actor
--data Action = LookAt (Maybe Item) | LookAt (Maybe Actor) deriving (Show)

data Action = LookAt Id | LookAround | PickUp Id | WalkTo Id | Combine Id Id

parseAction :: String -> Maybe Action
parseAction s = parseAction' (splitOn " " s)

parseAction' :: [String] -> Maybe Action
parseAction' (verb:[])
    | verb == "L" = Just LookAround
    | otherwise = Nothing
parseAction' (verb:id1:id2:ids) =
    case verb of
        "C" -> Just (Combine id1 id2)
        _ -> Nothing
parseAction' (verb:id:ids) =
    case verb of
        "L" -> Just (LookAt id)
        "P" -> Just (PickUp id)
        "W" -> Just (WalkTo id)
        _ -> Nothing



respondAction :: GameState -> Maybe Action -> ActionResult
respondAction gamestate Nothing = ActionResult gamestate "I don't know how to do that, señor.\n"
respondAction gamestate (Just action) = respondValidAction gamestate action

respondValidAction :: GameState -> Action -> ActionResult
respondValidAction gamestate LookAround = ActionResult gamestate (show gamestate)
respondValidAction gamestate@(GameState (Player roomId _) world) (LookAt id) = 
    lookAtSomething gamestate (findEntityById roomId world) id

respondValidAction gamestate@(GameState (Player roomId _) world) (PickUp id) =
    pickUpSomething gamestate (findEntityById roomId world) id

respondValidAction gamestate@(GameState (Player roomId _) world) (WalkTo exitId) =
    goSomewhere gamestate (findEntityById roomId world) exitId

respondValidAction gamestate@(GameState (Player roomId inventory) world) (Combine id1 id2) =
    combineSomething gamestate (findEntityById roomId world) id1 id2

combineSomething :: GameState -> Maybe Room -> Id -> Id -> ActionResult
combineSomething gamestate Nothing _ _ = ActionResult gamestate "Room error!"
combineSomething gamestate@(GameState (Player _ inventory) _) (Just (Room _ _ items _)) id1 id2 =
    combineSomething' gamestate (findEntityById id1 inventory) (findEntityById id2 (items ++ inventory))

combineSomething' :: GameState -> Maybe Item -> Maybe Item -> ActionResult
combineSomething' gamestate Nothing _ = ActionResult gamestate "You don't have that."
combineSomething' gamestate _ Nothing = ActionResult gamestate "You can't use this with that."
combineSomething' gamestate (Just item1) (Just item2) = combine gamestate item1 item2

getObservationsFromRoom :: Room -> [Observation]
getObservationsFromRoom (Room _ exits items actors) =
    (map toObservation exits) ++ (map toObservation items) ++ (map toObservation actors)

lookAtSomething' :: GameState -> Maybe String -> ActionResult
lookAtSomething' gamestate Nothing = ActionResult gamestate "You can't look at something that does not exist."
lookAtSomething' gamestate (Just s) = ActionResult gamestate s

lookAtSomething :: GameState -> Maybe Room -> Id -> ActionResult
lookAtSomething gamestate Nothing _ = ActionResult gamestate "Room ref error!"    
lookAtSomething gamestate@(GameState (Player _ inventory) _) (Just room) id =
    lookAtSomething' gamestate (findObservationById ((map toObservation inventory) ++ (getObservationsFromRoom room)) id)

pickUpSomething :: GameState -> Maybe Room -> Id -> ActionResult
pickUpSomething gamestate Nothing _ = ActionResult gamestate "Pickup failed -> No room."
pickUpSomething gamestate (Just room) id = pickUpSomething' gamestate (findItemInRoom room id)

pickUpSomething' :: GameState -> Maybe Item -> ActionResult
pickUpSomething' gamestate Nothing = ActionResult gamestate "How can you pick up that which does not exist?"
pickUpSomething' gamestate (Just item) =
    ActionResult (transferItemFromWorldToPlayer gamestate item) ("You picked up the " ++ (show item))

goSomewhere' :: GameState -> Maybe Room -> ActionResult
goSomewhere' gamestate Nothing = ActionResult gamestate "You can't go there."
goSomewhere' (GameState (Player _ inventory) world) (Just (Room roomId _ _ _)) =
    ActionResult newGameState (show newGameState)
    where newGameState = GameState (Player roomId inventory) world

goSomewhere :: GameState -> Maybe Room -> Id -> ActionResult
goSomewhere gamestate Nothing _ = ActionResult gamestate "Room ref error!"
goSomewhere gamestate (Just (Room _ exits _ _)) exitId =
    goSomewhere' gamestate (findEntityById exitId exits)

lookAt :: (Entity a) => Maybe a -> String
lookAt Nothing = "I can't look at that, señor."
lookAt (Just observable) = getDescription observable



bathroom =
    Room
        "Bathroom"
        [
            library
        ]
        [
            Item "Toothbrush" "A sparkling new toothbrush.",
            Item "Toilet" "It's one of them fancy toilets."
        ]
        [
            Actor "ToiletMan" [] "The great toilet man looms over thee."
        ]


library =
    Room 
        "Library"
        [          
            bathroom  
        ]
        [
            Item "Sword" "A beautiful, shining, freshly polished sword.",
            Item "Key" "It is a golden key.",
            Item "Book" "The title is Zob Goblin and the Poggle of Buckletwig."
        ]
        [
            Actor "YoungLad" [Item "Apple" "A red, shining apple."] "A fine, young lad."
        ]

jail = Room "Jail" 
    [] -- no exits initially, will exit to library when door is open
    [
        Item "JaildoorKey" "A rusty, iron key.",
        Item "ClosedJaildoor" "It's a steel bar door, impossible to break."
    ]
    []

sampleWorld = [library, bathroom, jail]
samplePlayer = Player "Jail" [Item "Gun" "It's a good, old Smith & Wesson."]

gameLoop :: GameState -> IO ()
gameLoop gamestate = do
    action <- getLine
    let actionResult = respondAction gamestate (parseAction action)
    putStrLn (getMessageFromActionResult actionResult)
    gameLoop (getGamestateFromActionResult actionResult)

main :: IO ()
main = do
    gameLoop (GameState samplePlayer sampleWorld)
