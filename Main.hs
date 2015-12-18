module Main where
    import Data.List
    import World
    import Actor
    import Room
    import Item

    main = do
        let a = Actor {}
        let i = Item { name = "Fork" }
        let r = Room { items = [], exits = [], actors = []}
        let w1 = World { rooms = [] }
        putStrLn $ show w1
        

        let w2 = World.addRoom w1 r
        putStrLn $ show $ length (rooms w2)
        putStrLn $ show w2

        let w3 = World.addToRoom w2 r (RoomContentItem i)
        putStrLn $ show w3
        putStrLn $ show r

        let w4 = World.addToRoom w3 r (RoomContentActor a)
        putStrLn $ show w4
        
        putStrLn "END"
