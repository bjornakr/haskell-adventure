module Action.WalkTo where
    import Entity
    import CoreTypes
    import Core
    
    walkTo :: Room -> Room -> GameState -> ActionResult
    walkTo fromRoom toRoom gamestate =
        case (getId fromRoom) of
            ("Bathroom") ->
                case (hasState "Player" "DirtyHands" gamestate) of
                    True -> unwashedHands gamestate
                    False -> ActionResult (show newGameState) newGameState

            _ -> ActionResult (show newGameState) newGameState
            where newGameState = movePlayerToRoom toRoom gamestate


    unwashedHands gamestate =
        ActionResult 
            ("You didn't wash your hands!!! The police catches you and puts you in jail.\n\n" 
                ++ (show newGameState)) newGameState
        where newGameState = (movePlayerToRoomId "Jail"
                               . addState "Jaildoor" "Locked"
                               . removeState "Player" "DirtyHands") gamestate
