module Room where
    import Item 
    import Actor

    data Room = Room { exits :: [Exit], items :: [Item], actors :: [Actor] } deriving (Show, Eq)
    data Exit = Exit { room :: Room } deriving (Show, Eq)

    connectRoom :: Room -> Exit -> Room
    connectRoom r e =
        r { exits = e : exits r }

    addItem :: Room -> Item -> Room
    addItem r i =
        r { items = i : items r }

    addActor :: Room -> Actor -> Room
    addActor r a =
        r { actors = a : actors r }

    