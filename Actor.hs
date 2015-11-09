module Actor where
	data Actor = Actor {} deriving (Show, Eq)

	--pickUp :: World -> Actor -> Room -> Item -> World
	--pickUp w a r i =
		-- make sure a is in r, r is in w, i is in r
