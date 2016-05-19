module Action.Combine where    
    import Entity
    import CoreTypes
    import Core

    combine' :: Item -> Item -> Bool-> GameState -> ActionResult
    combine' item1 item2 reversed =
        case (getId item1, getId item2) of
            ("Toothbrush", "Toilet") ->
                ActionResult "You give the toilet a good scrub."
                     . updateItem (LooseItem (ItemDetails "Toothbrush" "The toothbrush looks rather unappealing."))
                     . updateItem (StaticItem (ItemDetails "Toilet" "A super clean toilet"))

            ("JaildoorKey", "Jaildoor") ->
                ActionResult "You hear a click. Amazing, the key worked!"
                    . removeState  "Jaildoor" "Locked"
                    . destroyItem item1
            _ ->
                if reversed
                    then ActionResult "You cannot combine those items."
                    else combine' item2 item1 True

    combine :: Item -> Item -> GameState -> ActionResult
    combine i1 i2 = combine' i1 i2 False