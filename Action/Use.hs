module Action.Use where
    import Entity
    import Core

    use :: GameState -> Item -> ActionResult
    use gamestate item =
        case (getId item) of
            ("Gun") -> ActionResult gamestate "You are out of bullets."

            ("Toilet") -> ActionResult (addState gamestate "Player" "DirtyHands") "You relieve yourself of the pressure."

            ("WashBasin") -> ActionResult (removeState gamestate "Player" "DirtyHands") "You wash your hands thoroughly."

            _ -> ActionResult gamestate $ "You cannot use the " ++ (getId item)