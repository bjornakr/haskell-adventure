module Action.Core (ActionResult, respondAction) where
    import Data.List.Split
    import Entity
    -- do you need Core?
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

    respondValidAction' :: Maybe Room -> Action -> GameState -> ActionResult
    respondValidAction' Nothing _ gamestate = ActionResult "Room reference error!" gamestate

    respondValidAction' _ LookAround gamestate = ActionResult (show gamestate) gamestate

    respondValidAction' (Just room) (LookAt id) gamestate@(GameState (Player _ inventory) world _) =
        lookAtSomething             
            (findObservationById ((map toObservation inventory) ++ (getObservationsFromRoom room)) id)
            gamestate

    respondValidAction' (Just room) (PickUp id) gamestate =
        pickUpSomething (findItemInRoom room id) gamestate

    respondValidAction' (Just fromRoom@(Room _ exits _ _)) (WalkTo exitId) gamestate@(GameState _ world _)
        | elem exitId exits = goSomewhere fromRoom (findEntityById exitId world) gamestate
        | otherwise = ActionResult ("You cannot go to " ++ exitId) gamestate

    respondValidAction' (Just (Room _ _ items _)) (Combine id1 id2) gamestate@(GameState (Player _ inventory) _ _) =
        combineSomething (findEntityById id1 inventory) (findEntityById id2 (items ++ inventory)) gamestate

    respondValidAction' (Just (Room _ _ items _)) (Open id) gamestate@(GameState (Player _ inventory) _ _) =
        openSomething (findEntityById id (inventory ++ items)) gamestate

    respondValidAction' (Just (Room _ _ items _)) (Use id) gamestate@(GameState (Player _ inventory) _ _) =
        useSomething (findEntityById id (inventory ++ items)) gamestate

    respondValidAction' (Just (Room _ _ _ actors)) (TalkTo id) gamestate =
        talkToSomeone (findEntityById id actors) gamestate

    respondValidAction :: Action -> GameState -> ActionResult
    respondValidAction action gamestate@(GameState (Player roomId _) world _) =
        respondValidAction' (findEntityById roomId world) action gamestate

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

    goSomewhere :: Room -> Maybe Room -> GameState -> ActionResult
    goSomewhere _ Nothing = ActionResult "You can't go there."
    goSomewhere fromRoom (Just toRoom) = walkTo fromRoom toRoom


    --goSomewhere (GameState (Player _ inventory) world stateMap) (Just (Room roomId _ _ _)) =
    --    walkTo gamestate 
        --ActionResult newGameState (show newGameState)
        --where newGameState = GameState (Player roomId inventory) world stateMap

    openSomething :: Maybe Item -> GameState -> ActionResult 
    openSomething Nothing = ActionResult "You cannot open that."
    openSomething (Just item) = open item

    useSomething :: Maybe Item -> GameState -> ActionResult
    useSomething Nothing = ActionResult "You cannot use that."
    useSomething (Just item) = use item

    talkToSomeone :: Maybe Actor -> GameState -> ActionResult
    talkToSomeone Nothing = ActionResult "You cannot talk to that."
    talkToSomeone (Just actor) = talkTo actor
