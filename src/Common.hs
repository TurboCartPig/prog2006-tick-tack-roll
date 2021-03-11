module Common (headM, tailM, enumerate) where

-- | Maybe get the head of a list.
--
-- >>> headM [1..5]
-- Just 1
--
-- >>> headM []
-- Nothing
headM :: [a] -> Maybe a
headM [] = Nothing
headM (x : _) = Just x

-- | Maybe get the tail of a list.
--
-- >>> tailM [1..5]
-- Just [2,3,4,5]
--
-- >>> tailM [] :: Maybe [Int]
-- Nothing
tailM :: [a] -> Maybe [a]
tailM [] = Nothing
tailM (_ : xs) = Just xs

-- | Enumerate a list.
--
-- >>> enumerate "enumerate"
-- [(0,'e'),(1,'n'),(2,'u'),(3,'m'),(4,'e'),(5,'r'),(6,'a'),(7,'t'),(8,'e')]
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]
