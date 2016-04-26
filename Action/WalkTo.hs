module Action.WalkTo where
    import Entity
    import Core

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
