import Data.List.Split
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Data.Hashable (Hashable)

------------------------------------------------------------------------------------
-- TODO
--
-- Close
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
        --"There is someone here " ++ (show (map getId actors)) ++ ".\n\n" ++
        "Exits to " ++ (show exits) ++ ".\n\n"

data Player = Player RoomId Inventory deriving (Show, Eq)
type World = [Room]
type StateMap a = Map.HashMap a (Set.Set String)
data GameState = GameState Player World (StateMap String)
instance Show GameState where
    show (GameState (Player roomId inventory) world stateMap) =
        (observeEntity roomId world) ++
        "You have " ++ (show inventory) ++ ".\n\n" ++
        --"States: " ++ (show stateMap) ++ ".\n\n" ++
        "What would you like to do?\n" ++
        "[W]alk to | [L]ook (at) | [P]ick up | Co[M]bine\n" ++
        "[G]ive    | [T]alk to   | Pu[S]h    | Pu[L]l\n" ++
        "[O]pen    | [C]lose     | [U]se     |       \n"

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



combine' :: GameState -> Item -> Item -> Bool -> ActionResult
combine' gamestate item1 item2 reversed =
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
                
        ("JaildoorKey", "Jaildoor") ->
            ActionResult
                (removeState (destroyItem gamestate item1) "Jaildoor" "Locked")
                --(exchangeItem (removeItemFromWorld gamestate item1) item2 (StaticItem (ItemDetails "UnlockedClosedJaildoor" (getDescription item2))))
                "You hear a click. Amazing, the key worked!"
        _ ->
            case reversed of
                True -> ActionResult gamestate "You cannot combine those items."
                False -> combine' gamestate item2 item1 True

combine :: GameState -> Item -> Item -> ActionResult
combine gamestate i1 i2 = combine' gamestate i1 i2 False

open :: GameState -> Item -> ActionResult
open gamestate@(GameState (Player roomId inventory) world stateMap) item =
    case (getId item) of
        ("Jaildoor") -> 
            case (hasState stateMap "Jaildoor" "Locked") of
                True -> ActionResult gamestate "You try the door, but it is locked. Maybe there is a key somewhere..."
                False ->
                    ActionResult
                        (linkRooms (updateItemDescription gamestate item "The jaildoor is wide open.")
                            roomId "Library")                           
                        "You open the jaildoor."

        ("Box") ->
            ActionResult (updateItemDescription gamestate item "The box is open.") "You open the box."

        _ -> ActionResult gamestate ("You cannot open the " ++ (show item))


walkTo :: GameState -> Room -> Room -> ActionResult
walkTo gamestate@(GameState (Player _ inventory) world stateMap) fromRoom toRoom =
    case (getId fromRoom) of
        ("Bathroom") ->
            case (hasState stateMap "Player" "DirtyHands") of
                True -> ActionResult
                            newGameState
                            ("You didn't wash your hands!!! The police catches you and puts you in jail.\n\n" ++ (show newGameState))
                        where newGameState = (setPlayerRoom (addState (removeState gamestate "Player" "DirtyHands") "Jaildoor" "Locked") "Jail")
                False -> ActionResult newGameState (show newGameState)

        _ -> ActionResult newGameState (show newGameState)
        where newGameState = GameState (Player (getId toRoom) inventory) world stateMap


use :: GameState -> Item -> ActionResult
use gamestate item =
    case (getId item) of
        ("Gun") -> ActionResult gamestate "You are out of bullets."

        ("Toilet") -> ActionResult (addState gamestate "Player" "DirtyHands") "You relieve yourself of the pressure."

        ("WashBasin") -> ActionResult (removeState gamestate "Player" "DirtyHands") "You wash your hands thoroughly."

        _ -> ActionResult gamestate $ "You cannot use the " ++ (getId item)


------------------------------------------------------------------------------------
-- USE CASES / APPLICATION
------------------------------------------------------------------------------------

-- data Action = WalkTo Room | LookAt Item | PickUp Item | Use Item Item | Give Item Actor | TalkTo Actor
--data Action = LookAt (Maybe Item) | LookAt (Maybe Actor) deriving (Show)

data Action = LookAt Id | LookAround | PickUp Id | WalkTo Id | Combine Id Id | Open Id | Close Id | Use Id

parseAction :: String -> Maybe Action
parseAction s = parseAction' (splitOn " " s)

parseAction' :: [String] -> Maybe Action
parseAction' (verb:[])
    | verb == "L" = Just LookAround
    | otherwise = Nothing
parseAction' (verb:id1:id2:ids) =
    case verb of
        "M" -> Just (Combine id1 id2)
        _ -> Nothing
parseAction' (verb:id:ids) =
    case verb of
        "L" -> Just (LookAt id)
        "P" -> Just (PickUp id)
        "W" -> Just (WalkTo id)
        "O" -> Just (Open id)
        "C" -> Just (Close id)
        "U" -> Just (Use id)
        _ -> Nothing


respondAction :: GameState -> Maybe Action -> ActionResult
respondAction gamestate Nothing = ActionResult gamestate "I don't know how to do that, señor.\n"
respondAction gamestate (Just action) = respondValidAction gamestate action


respondValidAction' :: GameState -> Maybe Room -> Action -> ActionResult
respondValidAction' gamestate Nothing _ = ActionResult gamestate "Room reference error!"

respondValidAction' gamestate _ LookAround = ActionResult gamestate (show gamestate)

respondValidAction' gamestate@(GameState (Player _ inventory) world _) (Just room) (LookAt id) = 
    lookAtSomething 
        gamestate 
        (findObservationById ((map toObservation inventory) ++ (getObservationsFromRoom room world)) id)

respondValidAction' gamestate (Just room) (PickUp id) =
    pickUpSomething gamestate (findItemInRoom room id)

respondValidAction' gamestate@(GameState _ world _) (Just fromRoom@(Room _ exits _ _)) (WalkTo exitId)
    | elem exitId exits = goSomewhere gamestate fromRoom (findEntityById exitId world)
    | otherwise = ActionResult gamestate ("You cannot go to " ++ exitId)

respondValidAction' gamestate@(GameState (Player _ inventory) _ _) (Just (Room _ _ items _)) (Combine id1 id2) =
    combineSomething gamestate (findEntityById id1 inventory) (findEntityById id2 (items ++ inventory))

respondValidAction' gamestate@(GameState (Player _ inventory) _ _) (Just (Room _ _ items _)) (Open id) =
    openSomething gamestate (findEntityById id (inventory ++ items))

respondValidAction' gamestate@(GameState (Player _ inventory) _ _) (Just (Room _ _ items _)) (Use id) =
    useSomething gamestate (findEntityById id (inventory ++ items))

respondValidAction :: GameState -> Action -> ActionResult
respondValidAction gamestate@(GameState (Player roomId _) world _) action =
    respondValidAction' gamestate (findEntityById roomId world) action

combineSomething :: GameState -> Maybe Item -> Maybe Item -> ActionResult
combineSomething gamestate Nothing _ = ActionResult gamestate "You don't have that."
combineSomething gamestate _ Nothing = ActionResult gamestate "You can't combine those."
combineSomething gamestate (Just item1) (Just item2) = combine gamestate item1 item2

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
    ActionResult (transferItemFromWorldToPlayer gamestate item) ("You pick up the " ++ (show item))

goSomewhere :: GameState -> Room -> Maybe Room -> ActionResult
goSomewhere gamestate _ Nothing = ActionResult gamestate "You can't go there."
goSomewhere gamestate fromRoom (Just toRoom) =
    walkTo gamestate fromRoom toRoom


--goSomewhere (GameState (Player _ inventory) world stateMap) (Just (Room roomId _ _ _)) =
--    walkTo gamestate 
    --ActionResult newGameState (show newGameState)
    --where newGameState = GameState (Player roomId inventory) world stateMap

openSomething :: GameState -> Maybe Item -> ActionResult
openSomething gamestate Nothing = ActionResult gamestate "You cannot open that."
openSomething gamestate (Just item) = open gamestate item

useSomething :: GameState -> Maybe Item -> ActionResult
useSomething gamestate Nothing = ActionResult gamestate "You cannot use that."
useSomething gamestate (Just item) = use gamestate item

bathroom =
    Room
        "Bathroom"
        [
            "Library"
        ]
        [
            LooseItem (ItemDetails "Toothbrush" "A sparkling new toothbrush."),
            StaticItem (ItemDetails "Toilet" "It's one of them fancy toilets."),
            StaticItem (ItemDetails "WashBasin" "A plain old wash basin.")
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
        StaticItem (ItemDetails "Jaildoor" "It's a steel bar door, impossible to break."),
        StaticItem (ItemDetails "Box" "It's a closed box.")
    ]
    []

sampleWorld = [library, bathroom, jail]
samplePlayer = Player "Bathroom" [LooseItem (ItemDetails "Gun" "It's a good, old Smith & Wesson.")]
sampleStateMap = Map.singleton "Jaildoor" (Set.singleton "") --  [("Jaildoor", ["Locked"])]

gameLoop :: GameState -> IO ()
gameLoop gamestate = do
    action <- getLine
    let actionResult = respondAction gamestate (parseAction action)
    putStrLn (getMessageFromActionResult actionResult)
    gameLoop (getGamestateFromActionResult actionResult)

main :: IO ()
main = do
    gameLoop (GameState samplePlayer sampleWorld sampleStateMap)
