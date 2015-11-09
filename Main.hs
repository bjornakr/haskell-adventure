module Main where
    import Data.List
    import World
    import Room
    import Item

    main = do
        let i = Item { name = "Fork" }
        let r = Room { items = [], exits = [], actors = []}
        let w1 = World { rooms = [] }
        putStrLn $ show w1
        

        let w2 = World.addRoom w1 r
        putStrLn $ show $ length (rooms w2)
        putStrLn $ show w2

        let w3 = World.addItem w2 r i
        putStrLn $ show w3
        putStrLn $ show r
        putStrLn "END"
