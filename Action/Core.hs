module Action.Core (respondAction) where
    import Data.List.Split
    import Entity
    import Core 
    import Action.Combine
    import Action.WalkTo
    import Action.Open
    import Action.Use
    import Action.TalkTo

    data Action = 
        LookAt Id | LookAround | PickUp Id | WalkTo Id |
        Combine Id Id | Open Id | Close Id | Use Id |
        TalkTo Id

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
            "T" -> Just (TalkTo id)
            _ -> Nothing

    respondAction :: String -> GameState -> ActionResult
    respondAction actionString = case (parseAction actionString) of
            Nothing -> ActionResult "I don't know how to do that, señor.\n"
            (Just action) -> respondValidAction action

    respondValidAction :: Action -> GameState -> ActionResult
    respondValidAction action gamestate@(GameState (Player roomId _) world _) =
        case (findEntityById roomId world) of
            Just room   -> respondValidAction' room action gamestate
            Nothing     -> ActionResult "Room reference error!" gamestate

    respondValidAction' :: Room -> Action -> GameState -> ActionResult        
    respondValidAction' room@(Room _ _ items actors) action gamestate@(GameState (Player _ inventory) _ _) =
        case action of
            LookAround      -> ActionResult (show gamestate) gamestate
            LookAt id0      -> lookAtSomething (observe id0 room gamestate) gamestate
            PickUp id0      -> pickUpSomething (findItemInRoom room id0) gamestate
            WalkTo id0      -> goSomewhere room id0 gamestate
            Combine id1 id2 -> combineSomething (findEntityById id1 inventory) (findEntityById id2 (items ++ inventory)) gamestate
            Open id0        -> openSomething (findEntityById id0 (inventory ++ items)) gamestate
            Use id0         -> useSomething (findEntityById id0 (inventory ++ items)) gamestate
            TalkTo id0      -> talkToSomeone (findEntityById id0 actors) gamestate



    combineSomething :: Maybe Item -> Maybe Item -> GameState -> ActionResult
    combineSomething Nothing _ = ActionResult "You don't have that."
    combineSomething _ Nothing = ActionResult "You can't combine those."
    combineSomething (Just item1) (Just item2) = combine item1 item2

    getObservationsFromRoom :: Room -> [Observation]
    getObservationsFromRoom (Room _ exits items actors) = (map toObservation items) ++ (map toObservation actors)

    lookAtSomething :: Maybe String -> GameState -> ActionResult 
    lookAtSomething Nothing = ActionResult "You can't look at something that does not exist."
    lookAtSomething (Just s) = ActionResult s

    pickUpSomething :: Maybe Item -> GameState -> ActionResult
    pickUpSomething Nothing = ActionResult "How can you pick up that which does not exist?"
    pickUpSomething (Just item@(StaticItem _)) =
        ActionResult ("You cannot pick up the " ++ (getId item))
    pickUpSomething (Just item@(LooseItem _)) =
        ActionResult ("You pick up the " ++ (getId item))
        . transferItemFromWorldToPlayer item

    goSomewhere :: Room -> Id -> GameState -> ActionResult
    goSomewhere fromRoom@(Room _ exits _ _) exitId gamestate@(GameState _ world _)
        | elem exitId exits = case (findEntityById exitId world) of
                                (Just toRoom)   -> walkTo fromRoom toRoom gamestate
                                Nothing         -> noGo
        | otherwise = noGo 
        where noGo = ActionResult "You can't go there." gamestate

    openSomething :: Maybe Item -> GameState -> ActionResult 
    openSomething Nothing = ActionResult "You cannot open that."
    openSomething (Just item) = open item

    useSomething :: Maybe Item -> GameState -> ActionResult
    useSomething Nothing = ActionResult "You cannot use that."
    useSomething (Just item) = use item

    talkToSomeone :: Maybe Actor -> GameState -> ActionResult
    talkToSomeone Nothing = ActionResult "You cannot talk to that."
    talkToSomeone (Just actor) = talkTo actor

    observe :: Id -> Room -> GameState -> Maybe String
    observe id0 room (GameState (Player _ inventory) _ _)  =
        findObservationById ((map toObservation inventory) ++ (getObservationsFromRoom room)) id0
