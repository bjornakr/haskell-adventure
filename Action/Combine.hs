module Action.Combine where    
    import Entity
    import Core
    
    combine' :: GameState -> Item -> Item -> Bool -> ActionResult
    combine' gamestate item1 item2 reversed =
        case (getId item1, getId item2) of
            ("Toothbrush", "Toilet") ->
                ActionResult 
                    (updateItem 
                        (updateItem 
                            gamestate 
                            (StaticItem (ItemDetails "Toilet" "A super clean toilet"))
                        )
                        (LooseItem (ItemDetails "Toothbrush" "The toothbrush looks rather unappealing."))
                    )
                    "You give the toilet a good scrub."
                    
            ("JaildoorKey", "Jaildoor") ->
                ActionResult
                    (removeState (destroyItem gamestate item1) "Jaildoor" "Locked")
                    --(exchangeItem (removeItemFromWorld gamestate item1) item2 (StaticItem (ItemDetails "UnlockedClosedJaildoor" (getDescription item2))))
                    "You hear a click. Amazing, the key worked!"
            _ ->
                case reversed of
                    True -> ActionResult gamestate "You cannot combine those items."
                    False -> combine' gamestate item2 item1 True

    combine :: GameState -> Item -> Item -> ActionResult
    combine gamestate i1 i2 = combine' gamestate i1 i2 False