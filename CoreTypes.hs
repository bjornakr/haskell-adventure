module CoreTypes where
    import qualified Data.Map as Map
    import qualified Data.Set as Set
    import Entity
    import ConversationTypes

    type ActorId = String
    type ItemId = String
    type RoomId = String
    type Inventory = [Item]
    type Description = String



    data Actor = Actor ActorId [Item] Description deriving (Eq)

    instance Entity Actor where
        getId (Actor id0 _ _) = id0

    instance Show Actor where
        show (Actor _ _ desc) = desc



    data ItemDetails = ItemDetails ItemId Description deriving (Eq)
    data Item = LooseItem ItemDetails | StaticItem ItemDetails deriving (Eq)
    
    instance Show ItemDetails where
        show (ItemDetails _ desc) = desc
 
    instance Entity ItemDetails where
        getId (ItemDetails id0 _) = id0

    instance Show Item where
        show = show . getItemDetails

    instance Entity Item where
        getId = getId . getItemDetails

    getItemDetails :: Item -> ItemDetails
    getItemDetails (LooseItem itemDetails) = itemDetails
    getItemDetails (StaticItem itemDetails) = itemDetails


    data Room = Room Id [RoomId] [Item] [Actor] deriving (Eq)

    instance Entity Room where
        getId (Room id0 _ _ _) = id0

    instance Show Room where
        show (Room id0 exitIds items actors) = 
            "You are in " ++ id0 ++ ".\n\n" ++
            "You see " ++ (show (map getId items)) ++ ".\n\n" ++
            "There is someone here " ++ (show (map getId actors)) ++ ".\n\n" ++
            "Exits to " ++ (show exitIds) ++ ".\n\n"


    data Player = Player RoomId Inventory deriving (Show, Eq)

    type World = [Room]

    type StateMap = Map.Map String (Set.Set String) -- Enables several states per key/id.
    
    data GameState = GameState {
        player :: Player,
        world :: World,
        stateMap :: StateMap,
        conversationState :: ConversationState
    }

    instance Show GameState where
        show (GameState { player = (Player roomId inventory), world = rooms, stateMap = stateMap }) =
            case (findEntityById roomId rooms) of
                Nothing -> error $ "Missing room: " ++ roomId
                (Just room) ->
                    (show room) ++
                    "You have " ++ (show (map getId inventory)) ++ "."
