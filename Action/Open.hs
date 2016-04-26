module Action.Open where
    import Entity
    import Core

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
            