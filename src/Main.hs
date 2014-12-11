module Main where

main :: IO ()
main = do
    print $ fmap (*2) (1 :> [2 :> [], 3 :> []])

    print $ size (fmap leaves (fmap (:> []) tree))
    print $ f tree
    print $ round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs
    print $ unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4))
    print $ unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))
    print $ foldl (*) 1 $ toList tree14 -- ???
    print $ unProduct $ fold tree14' -- ???


------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a
root (a :> rs) = a

children :: Rose a -> [Rose a]
children (_ :> rs) = rs

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

tree'' = 'x' :> map (\c -> c :> []) ['a'..'A']

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (_ :> rs) = 1 + (sum $ map size rs)

leaves :: Rose a -> Int
leaves (r :> []) = 1
leaves (_ :> (rs)) = sum $ map leaves rs

tree = 1 :> map (\c -> c :> []) [1..5]

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap f (r :> rs) = (f r) :> (map (\sr -> fmap f sr) rs)

f :: Rose a -> Rose a
f r = fmap head $ fmap (\x -> [x]) r

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum a) (Sum b) = Sum (a + b)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product a) (Product b) = Product (a * b)

unSum :: Sum a -> a
unSum (Sum a) = a
unProduct :: Product a -> a
unProduct (Product a) = a

seven :: Num string => Sum string
seven = Sum 3 `mappend` Sum 4

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))

num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))

ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap = error "you have to implement foldMap"

toList (r :> rs) = r : (concat $ map toList rs)

instance Foldable Rose where
  fold rose = foldr (mappend) mempty $ toList rose

tree14 = 1 :> [2 :> [], 3 :> [4 :> []]]
tree14' = fmap Product tree

sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum = error "you have to implement fsum"
fproduct = error "you have to implement fproduct"

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

