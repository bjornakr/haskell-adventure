module World where
    import Data.List
    import Room
    import Item

    data World = World { rooms :: [Room] } deriving (Show, Eq)

    addRoom :: World -> Room -> World
    addRoom w r =
        w { rooms = r : rooms w }

    --addItem :: World -> Room -> Item -> World
    --addItem w r i =
    --    w { rooms = (Room.addItem r i) : (delete r (rooms w)) }

    addToRoom :: World -> Room -> RoomContent -> World
    addToRoom w r c =
        w { rooms = (Room.addContent r c) : (delete r (rooms w)) }
