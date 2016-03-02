import Data.List
import Data.List.Split

------------------------------------------------------------------------------------
-- TODO
--
-- Open / Close
------------------------------------------------------------------------------------



------------------------------------------------------------------------------------
-- DEFINITIONS
------------------------------------------------------------------------------------

type Id = String
type RoomId = Id
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


data Item = LooseItem ItemDetails | StaticItem ItemDetails deriving (Eq)
getItemDetails :: Item -> ItemDetails
getItemDetails (LooseItem itemDetails) = itemDetails
getItemDetails (StaticItem itemDetails) = itemDetails

instance Show Item where
    show item = show (getItemDetails item)
instance Entity Item where
    getId item = getId (getItemDetails item)    
    getDescription item = getDescription (getItemDetails item)

data ItemDetails = ItemDetails Id Description deriving (Eq)
instance Show ItemDetails where
    show (ItemDetails id _) = id
instance Entity ItemDetails where
    getId (ItemDetails id _) = id
    getDescription (ItemDetails _ desc) = desc

data Room = Room Id [RoomId] [Item] [Actor] deriving (Show, Eq)
instance Entity Room where
    getId (Room id _ _ _) = id
    getDescription (Room id exits items actors) = 
        "You are in " ++ id ++ ".\n\n" ++
        "You see " ++ (show items) ++ ".\n\n" ++
        "There is someone here " ++ (show (map getId actors)) ++ ".\n\n" ++
        "Exits to " ++ (show exits) ++ ".\n\n"

data Player = Player RoomId Inventory deriving (Show, Eq)
type World = [Room]
data GameState = GameState Player World
instance Show GameState where
    show (GameState (Player roomId inventory) world) =
        (observeEntity roomId world) ++
        "You have " ++ (show inventory) ++ ".\n\n" ++
        "What would you like to do?\n" ++
        "[W]alk to | [L]ook (at) | [P]ick up | [U]se\n" ++
        "[G]ive    | [T]alk to   | Pu[S]h    | Pu[L]l\n" ++
        "[O]pen    | [C]lose     |           |       \n"

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

maybeRoomsToRooms :: Entity a => [Maybe a] -> [a]
maybeRoomsToRooms [] = []
maybeRoomsToRooms (Nothing:xs) = maybeRoomsToRooms xs
maybeRoomsToRooms ((Just x):xs) = x:(maybeRoomsToRooms xs)

------------------------------------------------------------------------------------
-- DOMAIN FUNCTIONS
------------------------------------------------------------------------------------

getGamestateFromActionResult :: ActionResult -> GameState
getGamestateFromActionResult (ActionResult gamestate _) = gamestate

getMessageFromActionResult :: ActionResult -> String
getMessageFromActionResult (ActionResult _ message) = message

--linkRooms :: World -> Room -> Room -> World
--linkRooms w room1@(Room rid1 rs1 i1 a1) room2@(Room rid2 rs2 i2 a2) =
--    addRooms w [Room rid1 (room2:rs1) i1 a1, Room rid2 (room1:rs2) i2 a2]

--addRooms :: World -> [Room] -> World
--addRooms w [] = w
--addRooms w (r:rs) = addRooms (addRoom w r) rs

--addRoom :: World -> Room -> World
--addRoom world room = addOrUpdateEntity room world

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


use' :: GameState -> Item -> Item -> Bool -> ActionResult
use' gamestate item1 item2 reversed =
    case (getId item1, getId item2) of
        ("Toothbrush", "Toilet") ->
            ActionResult 
                (updateItem 
                    (updateItem 
                        gamestate 
                        (StaticItem (ItemDetails "Toilet" "A super clean toilet"))
                    )
                    (LooseItem (ItemDetails "Toothbrush" "The toothbrush looks rather unappealing."))
                )
                "You give the toilet a good scrub."
        ("JaildoorKey", "LockedClosedJaildoor") ->
            ActionResult
                (exchangeItem (removeItemFromWorld gamestate item1) item2 (StaticItem (ItemDetails "UnlockedClosedJaildoor" (getDescription item2))))
                "You hear a click. Amazing, the key worked!"
        _ ->
            case reversed of
                True -> ActionResult gamestate "You cannot combine those items."
                False -> use' gamestate item2 item1 True

use :: GameState -> Item -> Item -> ActionResult
use gamestate i1 i2 = use' gamestate i1 i2 False

open :: GameState -> Item -> ActionResult
open gamestate item =
    case (getId item) of
        ("UnlockedClosedJaildoor") ->
            ActionResult
                (exchangeItem gamestate item (StaticItem (ItemDetails "OpenJaildoor" "The jaildoor is wide open")))
                "You open the jaildoor."

        ("LockedClosedJaildoor") -> ActionResult gamestate "You try the door, but it is locked. Maybe there is a key somewhere..."

        _ -> ActionResult gamestate ("You cannot open the " ++ (show item))


--combine :: GameState -> Item -> Item -> ActionResult
--combine gamestate (Item "Toothbrush" _ _) (Item "Toilet" _ _) = 
--    ActionResult 
--        (updateItem (updateItem gamestate (Item "Toilet" "A super clean toilet")) (Item "Toothbrush" "The toothbrush looks rather unappealing."))
--        "You give the toilet a good scrub."

--combine gamestate i1@(Item "Toilet" _ _) i2@(Item "Toothbrush" _ _) = combine gamestate i2 i1

--combine gamestate jaildoorKey@(Item "JaildoorKey" _ _) closedJaildoor@(Item "ClosedJaildoor" jaildoorDescription _) = 
--    ActionResult
--        (exchangeItem (removeItemFromWorld gamestate jaildoorKey) closedJaildoor (Item "OpenJaildoor" jaildoorDescription))
--        "You hear a click. Amazing, the key worked!"

--combine gamestate i1@(Item "ClosedJaildoor" _ _) i2@(Item "JaildoorKey" _ _) = combine gamestate i2 i1

--combine gamestate _ _ = ActionResult gamestate "You cannot combine those items."





------------------------------------------------------------------------------------
-- USE CASES / APPLICATION
------------------------------------------------------------------------------------

-- data Action = WalkTo Room | LookAt Item | PickUp Item | Use Item Item | Give Item Actor | TalkTo Actor
--data Action = LookAt (Maybe Item) | LookAt (Maybe Actor) deriving (Show)

data Action = LookAt Id | LookAround | PickUp Id | WalkTo Id | Use Id Id | Open Id | Close Id

parseAction :: String -> Maybe Action
parseAction s = parseAction' (splitOn " " s)

parseAction' :: [String] -> Maybe Action
parseAction' (verb:[])
    | verb == "L" = Just LookAround
    | otherwise = Nothing
parseAction' (verb:id1:id2:ids) =
    case verb of
        "U" -> Just (Use id1 id2)
        _ -> Nothing
parseAction' (verb:id:ids) =
    case verb of
        "L" -> Just (LookAt id)
        "P" -> Just (PickUp id)
        "W" -> Just (WalkTo id)
        "O" -> Just (Open id)
        "C" -> Just (Close id)
        _ -> Nothing



respondAction :: GameState -> Maybe Action -> ActionResult
respondAction gamestate Nothing = ActionResult gamestate "I don't know how to do that, señor.\n"
respondAction gamestate (Just action) = respondValidAction gamestate action


respondValidAction' :: GameState -> Maybe Room -> Action -> ActionResult
respondValidAction' gamestate Nothing _ = ActionResult gamestate "Room reference error!"

respondValidAction' gamestate _ LookAround = ActionResult gamestate (show gamestate)

respondValidAction' gamestate@(GameState (Player _ inventory) world) (Just room) (LookAt id) = 
    lookAtSomething gamestate (findObservationById ((map toObservation inventory) ++ (getObservationsFromRoom room world)) id)

respondValidAction' gamestate (Just room) (PickUp id) =
    pickUpSomething gamestate (findItemInRoom room id)

respondValidAction' gamestate@(GameState _ world) (Just (Room _ exits _ _)) (WalkTo exitId)
    | elem exitId exits = goSomewhere gamestate (findEntityById exitId world)
    | otherwise = ActionResult gamestate ("You cannot go to " ++ exitId)
    --ActionResult gamestate ("ExitID " ++ exitId ++ " - Exits: " ++ (show exits))

respondValidAction' gamestate@(GameState (Player _ inventory) _) (Just (Room _ _ items _)) (Use id1 id2) =
    useSomething gamestate (findEntityById id1 inventory) (findEntityById id2 (items ++ inventory))

respondValidAction' gamestate@(GameState (Player _ inventory) _) (Just (Room _ _ items _)) (Open id) =
    openSomething gamestate (findEntityById id (inventory ++ items))


respondValidAction :: GameState -> Action -> ActionResult
respondValidAction gamestate@(GameState (Player roomId _) world) action =
    respondValidAction' gamestate (findEntityById roomId world) action

useSomething :: GameState -> Maybe Item -> Maybe Item -> ActionResult
useSomething gamestate Nothing _ = ActionResult gamestate "You don't have that."
useSomething gamestate _ Nothing = ActionResult gamestate "You can't use this with that."
useSomething gamestate (Just item1) (Just item2) = use gamestate item1 item2

getObservationsFromRoom :: Room -> World -> [Observation]
getObservationsFromRoom (Room _ exits items actors) world =
    (map toObservation (maybeRoomsToRooms (map (flip findEntityById world) exits))) ++ (map toObservation items) ++ (map toObservation actors)

lookAtSomething :: GameState -> Maybe String -> ActionResult
lookAtSomething gamestate Nothing = ActionResult gamestate "You can't look at something that does not exist."
lookAtSomething gamestate (Just s) = ActionResult gamestate s

pickUpSomething :: GameState -> Maybe Item -> ActionResult
pickUpSomething gamestate Nothing = ActionResult gamestate "How can you pick up that which does not exist?"
pickUpSomething gamestate (Just item@(StaticItem _)) =
    ActionResult gamestate ("You cannot pick up the " ++ (show item))
pickUpSomething gamestate (Just item@(LooseItem _)) =
    ActionResult (transferItemFromWorldToPlayer gamestate item) ("You picked up the " ++ (show item))

goSomewhere :: GameState -> Maybe Room -> ActionResult
goSomewhere gamestate Nothing = ActionResult gamestate "You can't go there."
goSomewhere (GameState (Player _ inventory) world) (Just (Room roomId _ _ _)) =
    ActionResult newGameState (show newGameState)
    where newGameState = GameState (Player roomId inventory) world

openSomething :: GameState -> Maybe Item -> ActionResult
openSomething gamestate Nothing = ActionResult gamestate "You cannot open that."
openSomething gamestate (Just item) = open gamestate item

bathroom =
    Room
        "Bathroom"
        [
            "Library"
        ]
        [
            LooseItem (ItemDetails "Toothbrush" "A sparkling new toothbrush."),
            StaticItem (ItemDetails "Toilet" "It's one of them fancy toilets.")
        ]
        [
            Actor "ToiletMan" [] "The great toilet man looms over thee."
        ]


library =
    Room 
        "Library"
        [          
            "Bathroom"
        ]
        [
            LooseItem (ItemDetails "Sword" "A beautiful, shining, freshly polished sword."),
            LooseItem (ItemDetails "Key" "It is a golden key."),
            LooseItem (ItemDetails "Book" "The title is Zob Goblin and the Poggle of Buckletwig.")
        ]
        [
            Actor "YoungLad" [LooseItem (ItemDetails "Apple" "A red, shining apple.")] "A fine, young lad."
        ]

jail = Room "Jail" 
    [] -- no exits initially, will exit to library when door is open
    [
        LooseItem (ItemDetails "JaildoorKey" "A rusty, iron key."),
        StaticItem (ItemDetails "LockedClosedJaildoor" "It's a steel bar door, impossible to break.")
    ]
    []

sampleWorld = [library, bathroom, jail]
samplePlayer = Player "Jail" [LooseItem (ItemDetails "Gun" "It's a good, old Smith & Wesson.")]

gameLoop :: GameState -> IO ()
gameLoop gamestate = do
    action <- getLine
    let actionResult = respondAction gamestate (parseAction action)
    putStrLn (getMessageFromActionResult actionResult)
    gameLoop (getGamestateFromActionResult actionResult)

main :: IO ()
main = do
    gameLoop (GameState samplePlayer sampleWorld)
