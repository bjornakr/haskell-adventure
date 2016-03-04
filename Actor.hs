module Actor where
	data Actor = Actor [Item] deriving (Show, Eq)

	--pickUp :: World -> Actor -> Room -> Item -> World
	--pickUp w a r i =
		-- make sure a is in r, r is in w, i is in r

	pickUp :: World -> Actor -> Item -> Maybe World
	pickUp w a i
		| getRoomOf a != getRoomOf i = Nothing
		| Just World { World.removeFromRoom i