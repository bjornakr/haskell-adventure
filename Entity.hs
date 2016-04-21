module Entity where
    type Id = String

    class (Eq a) => Entity a where
        idEq :: a -> a -> Bool
        getId :: a -> Id
        getDescription :: a -> String
        idEq a1 a2 = getId a1 == getId a2

    addOrUpdateEntity :: (Entity a, Eq a) => a -> [a] -> [a]
    addOrUpdateEntity e es
        | existingEntity == Nothing = e:es
        | otherwise = updateEntity e es
        where
            existingEntity = findEntity e es

    findEntity :: Entity a => a -> [a] -> Maybe a
    findEntity e [] = Nothing
    findEntity e' (e:es)
        | idEq e' e = Just e'
        | otherwise = findEntity e' es

    findEntityById :: Entity a => Id -> [a] -> Maybe a
    findEntityById id [] = Nothing
    findEntityById id (e:es)
        | id == (getId e) = Just e
        | otherwise = findEntityById id es

    updateEntity :: Entity a => a -> [a] -> [a]
    updateEntity ue [] = []
    updateEntity ue (e:es)
        | idEq ue e = ue:es
        | otherwise = e:(updateEntity ue es)


    removeEntity :: Entity a => a -> [a] -> [a]
    removeEntity e [] = []
    removeEntity e' (e:es)
        | idEq e' e = es
        | otherwise = e:(removeEntity e' es)

    exchangeEntity :: Entity a => a -> a -> [a] -> [a]
    exchangeEntity _ _ [] = []
    exchangeEntity oldEntity newEntity (e:es)
        | idEq oldEntity e = newEntity:es
        | otherwise = e:(exchangeEntity oldEntity newEntity es)


    observeEntity' :: Entity a => Maybe a -> String
    observeEntity' Nothing = ""
    observeEntity' (Just e) = getDescription e

    observeEntity :: Entity a => Id -> [a] -> String
    observeEntity id entities = observeEntity' (findEntityById id entities)
