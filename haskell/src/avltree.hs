data Tree a = EmptyTree | Node a Bal (Tree a) (Tree a) deriving (Show, Read, Eq)
data Bal = Same | Less | More deriving (Show, Read, Ord, Eq)

insert :: (Ord a) => a -> (Bal, Tree a) -> (Bal, Tree a)
insert a (_, EmptyTree) = more $ Node a Same EmptyTree EmptyTree
insert a (_, (Node x b left right))
    | a == x = same $ Node x b left right
    | a < x = case insert a (Same, left) of
                (Same, newLeft) -> same $ Node x b newLeft right
                (More, newLeft) -> case b of
                                    Same -> more $ Node x More newLeft right
                                    Less -> same $ Node x Same newLeft right
                                    More -> rotR x newLeft right
    | a > x = case insert a (Same, right) of
                (Same, newRight) -> same $ Node x b left newRight
                (More, newRight) -> case b of
                                    Same -> more $ Node x Less left newRight
                                    More -> same $ Node x Same left newRight
                                    Less -> rotL x left newRight

rotR :: a -> Tree a -> Tree a -> (Bal, Tree a)
rotR v (Node lv Same ll lr) r = more $ Node lv Less ll (Node v More lr r)
rotR v (Node lv More ll lr) r = same $ Node lv Same ll (Node v Same lr r)
rotR v (Node lv Less ll (Node lrv Same lrl lrr)) r = same $ Node lrv Same (Node lv Same ll lrl) (Node v Same lrr r)
rotR v (Node lv Less ll (Node lrv Less lrl lrr)) r = same $ Node lrv Same (Node lv More ll lrl) (Node v Same lrr r)
rotR v (Node lv Less ll (Node lrv More lrl lrr)) r = same $ Node lrv Same (Node lv Same ll lrl) (Node v Less lrr r)


rotL :: a -> Tree a -> Tree a -> (Bal, Tree a)
rotL v l (Node rv Same rl rr) = more $ Node rv More (Node v Less l rl) rr
rotL v l (Node rv Less rl rr) = same $ Node rv Same (Node v Same l rl) rr
rotL v l (Node rv More (Node rlv Same rll rlr) rr) = same $ Node rv Same (Node v Same l rll) (Node rlv Same rlr rr)
rotL v l (Node rv More (Node rlv Less rll rlr) rr) = same $ Node rv Same (Node v More l rll) (Node rlv Same rlr rr)
rotL v l (Node rv More (Node rlv More rll rlr) rr) = same $ Node rv Same (Node v Same l rll) (Node rlv Less rlr rr)


find :: (Ord a) => a -> Tree a -> Bool
find a EmptyTree = False
find a (Node x b left right)
    | a == x = True
    | a < x = find a left
    | a > x = find a right

same :: t -> (Bal, t)
same x = (Same, x)

more :: t -> (Bal, t)
more x = (More, x)

less :: t -> (Bal, t)
less x = (Less, x)
