module Action.Use where
    import Entity
    import Core
    
    use :: Item -> GameState -> ActionResult
    use item gamestate =
        case (getId item) of
            ("Gun") -> ActionResult "You are out of bullets." gamestate

            ("Toilet") -> (ActionResult "You relieve yourself of the pressure."
                            . addState "Player" "DirtyHands") gamestate

            ("WashBasin") -> (ActionResult "You wash your hands thoroughly."
                                . removeState "Player" "DirtyHands") gamestate

            _ -> ActionResult ("You cannot use the " ++ (getId item)) gamestate