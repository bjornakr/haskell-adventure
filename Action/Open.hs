module Action.Open where
    import Entity
    import CoreTypes
    import Core
    
    open :: Item -> GameState -> ActionResult
    open item gamestate =
        case getId item of
            "Jaildoor" -> 
                if hasState "Jaildoor" "Locked" gamestate
                    then ActionResult "You try the door, but it is locked. Maybe there is a key somewhere..." gamestate
                    else (ActionResult "You open the jaildoor."
                                . linkRooms "Jail" "Library"
                                . updateItemDescription item "The jaildoor is wide open.") gamestate

            "Box" ->
                (ActionResult "You open the box."
                    . updateItemDescription item "The box is open.") gamestate

            _ -> ActionResult ("You cannot open the " ++ show item) gamestate
            