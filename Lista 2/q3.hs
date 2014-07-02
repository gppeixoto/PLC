data Tree t = Node t (Maybe (Tree t)) (Maybe (Tree t)) deriving (Show, Eq)

trim :: (Ord t) => t -> t -> Maybe (Tree t) -> Maybe (Tree t)
trim _ _ Nothing = Nothing
trim min max (Just (Node val left right))
 | ((min <= val) && (max >= val)) = Just (Node val (trim min max left) (trim min max right))
 | (min > val) = trim min max right
 | (val > max) = trim min max left

symmetricaux :: (Maybe (Tree t)) -> (Maybe (Tree t)) -> Bool
symmetricaux Nothing Nothing = True
symmetricaux _ Nothing = False
symmetricaux Nothing _ = False
symmetricaux (Just (Node _ v1 v2)) (Just (Node _ u1 u2)) = (symmetricaux v1 u2) && (symmetricaux v2 u1) 

symmetric :: (Tree t) -> Bool
symmetric (Node _ left right) = symmetricaux left right

exchange :: Maybe (Tree t) -> Maybe (Tree t)
exchange Nothing = Nothing
exchange (Just (Node v l r)) = Just $ Node v r l

getval :: Maybe (Tree t) -> Maybe t
getval Nothing = Nothing
getval (Just (Node v l r)) = (Just v)

isoaux :: (Eq t) => Bool -> Maybe (Tree t) -> Maybe (Tree t) -> Bool    
isoaux _ Nothing Nothing = True
isoaux _ (Just (Node v Nothing Nothing)) (Just (Node v2 Nothing Nothing)) = (v == v2)
isoaux b (Just (Node v l r)) (Just (Node v2 l2 r2))
 | (v == v2) = if ((getval l == getval l2) && (getval r == getval r2)) then (isoaux b l l2) && (isoaux b r r2) else ( if b == False then (isoaux True (exchange (Just (Node v l r))) (Just (Node v2 l2 r2))) else False)
 | otherwise = False

isomorphic :: (Eq t) => Tree t -> Tree t -> Bool
isomorphic x y = isoaux False (Just x) (Just y)