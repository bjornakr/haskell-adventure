module Action.Core where
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

    respondValidAction' gamestate (Just (Room _ _ _ actors)) (TalkTo id) =
        talkToSomeone gamestate (findEntityById id actors)

    respondValidAction :: GameState -> Action -> ActionResult
    respondValidAction gamestate@(GameState (Player roomId _) world _) action =
        respondValidAction' gamestate (findEntityById roomId world) action

    combineSomething :: GameState -> Maybe Item -> Maybe Item -> ActionResult
    combineSomething gamestate Nothing _ = ActionResult gamestate "You don't have that."
    combineSomething gamestate _ Nothing = ActionResult gamestate "You can't combine those."
    combineSomething gamestate (Just item1) (Just item2) = combine gamestate item1 item2

    getObservationsFromRoom :: Room -> World -> [Observation]
    getObservationsFromRoom (Room _ exits items actors) world =
        (map toObservation items) ++ (map toObservation actors)

    lookAtSomething :: GameState -> Maybe String -> ActionResult
    lookAtSomething gamestate Nothing = ActionResult gamestate "You can't look at something that does not exist."
    lookAtSomething gamestate (Just s) = ActionResult gamestate s

    pickUpSomething :: GameState -> Maybe Item -> ActionResult
    pickUpSomething gamestate Nothing = ActionResult gamestate "How can you pick up that which does not exist?"
    pickUpSomething gamestate (Just item@(StaticItem _)) =
        ActionResult gamestate ("You cannot pick up the " ++ (getId item))
    pickUpSomething gamestate (Just item@(LooseItem _)) =
        ActionResult (transferItemFromWorldToPlayer gamestate item) ("You pick up the " ++ (getId item))

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

    talkToSomeone :: GameState -> Maybe Actor -> ActionResult
    talkToSomeone gamestate Nothing = ActionResult gamestate "You cannot talk to that."
    talkToSomeone gamestate (Just actor) = talkTo gamestate actor
