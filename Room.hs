module Room where
    import Item 
    import Actor

    data Room = Room { exits :: [Exit], items :: [Item], actors :: [Actor] } deriving (Show, Eq)
    data Exit = Exit { room :: Room } deriving (Show, Eq)
    data RoomContent = RoomContentItem Item | RoomContentActor Actor | RoomContentExit Exit


    connectRoom :: Room -> Exit -> Room
    connectRoom r e =
        r { exits = e : exits r }

    addItem :: Room -> Item -> Room
    addItem r i =
        r { items = i : items r }

    addActor :: Room -> Actor -> Room
    addActor r a =
        r { actors = a : actors r }

    addExit :: Room -> Exit -> Room
    addExit r e =
        r { exits = e : exits r }

    addContent :: Room -> RoomContent -> Room
    addContent r c =
        case c of 
            RoomContentItem c -> addItem r c
            RoomContentActor c -> addActor r c
            RoomContentExit c -> addExit r c
