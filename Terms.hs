module Terms where

import Data.Maybe
import Test.QuickCheck

data Name = X | Y
            deriving (Show, Eq)

data Term = B Int | F Name | Abs Term | App Term Term
            deriving (Show, Eq)

subst :: Term -> Name -> Term -> Term
subst (B n)       v s = B n 
subst (F x)       v s = if x == v then s else F x
subst (Abs t)     v s = Abs (subst t v s)
subst (App t1 t2) v s = App (subst t1 v s) (subst t2 v s)

substM :: Term -> Name -> Term -> Term
substM t v s = fromMaybe t (iter t) where
  
  iter :: Term -> Maybe Term
  iter (B _)    = Nothing
  iter (F x)
    | x == v    = Just s
    | otherwise = Nothing
                  
  iter (Abs t0)    = Abs `fmap` (iter t0)
  iter (App t1 t2) = 
    case (iter t1, iter t2) of
      (Nothing , Nothing) -> Nothing
      (m1 , m2)           -> Just $ App (fromMaybe t1 m1) (fromMaybe t2 m2)



substS :: Term -> Name -> Term -> Term
substS t v s = iter t id (\() -> t) where
  iter :: Term -> (Term -> Term) -> (() -> Term) -> Term
  iter t changed same =
    case t of
      B n -> same ()
      F x -> if x == v then
               changed s
             else
               same ()
      Abs t0    -> iter t0 (\t0' -> changed (Abs t0'))
                           same      -- not \() -> t   
      App t1 t2 -> iter t1 (\t1' -> iter t2 (\t2' -> changed (App t1' t2'))
                                            (\()  -> changed (App t1' t2)))
                           (\()  -> iter t2 (\t2' -> changed (App t1 t2'))
                                            same) -- not \() -> t
                   

testT :: Term -> Name -> Term -> Bool
testT t v s = subst t v s == substS t v s

testT2 :: Term -> Name -> Term -> Bool
testT2 t v s = subst t v s == substM t v s

instance Arbitrary Name where
  arbitrary = elements [X , Y]

instance Arbitrary Term where
  arbitrary = do
    n <- choose (0,3) :: Gen Int
    case n of
      0 -> fmap B $ elements [1, 2]
      1 -> fmap F $ arbitrary
      2 -> fmap Abs arbitrary
      3 -> do
        t1 <- arbitrary
        t2 <- arbitrary
        return $ App t1 t2
        

runTest :: IO ()
runTest = quickCheck testT