{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Symbolic where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Text.Printf
import Test.QuickCheck

type Var = Char

data Expr = V Var                -- variable
          | C Int                -- constant
          | Expr :+: Expr        -- sum of expressions
            deriving Eq          -- only for testing!

infixl :+:

eval :: Expr -> (Var -> Int) -> Int                                
eval (V v)       env = env v
eval (C n)       env = n
eval (e1 :+: e2) env = eval e1 env + eval e2 env

------------------------
--  Useful instances  --
------------------------

instance Show Expr where
  show (V v)       = [v]
  show (C c)       = show c
  show (e1 :+: e2) = printf "(%s + %s)" (show e1) (show e2)
  
-------------------------------------
--  Simplification of expressions  --
-------------------------------------

-- Maybe flavored version

simplify :: Expr -> Expr
simplify e = fromMaybe e (simplifyM e)

-- this function performs deep simplification while sharing as
-- much of the original structures as possible
-- this could prove useful when we'd have to keep a history of
-- all expressions (i.e. for undo) in a symbolic manipulation 
-- software

-- when the given expression is simplified to e' the result is Just e'
-- when no simplification was possible, Nothing is returned
simplifyM :: Expr -> Maybe Expr
simplifyM (V _)       = Nothing
simplifyM (C _)       = Nothing
simplifyM (e1 :+: e2) = 
  (case (s1, s2) of
    (Nothing, Nothing) -> simplPlusM e1 e2
    (_ , _ )           -> simplPlusM e1' e2' `mplus` Just (e1' :+: e2')) where
    
      s1 = simplifyM e1
      s2 = simplifyM e2
      
      e1' = fromMaybe e1 s1
      e2' = fromMaybe e2 s2
    
{- a more verbose rewrite of the :+: case, closer to the cps version 

  case (simplifyM e1, simplifyM e2) of
    (Nothing  , Nothing)  -> simplPlusM e1  e2  `mplus` Nothing
    (Just e1' , Nothing)  -> simplPlusM e1' e2  `mplus` Just (e1' :+: e2)
    (Nothing  , Just e2') -> simplPlusM e1  e2' `mplus` Just (e1  :+: e2') 
    (Just e1' , Just e2') -> simplPlusM e1' e2' `mplus` Just (e1' :+: e2')
-}

-- given two fully simplified operands of :+: tries to simplify
-- the whole expression. 
simplPlusM :: Expr -> Expr -> Maybe Expr
simplPlusM (C 0) e     = Just e
simplPlusM e     (C 0) = Just e
simplPlusM (C m) (C n) = Just $ C (m + n)
simplPlusM _      _    = Nothing

-- CPS flavored version

simplify2 :: Expr -> Expr
simplify2 e = simplifyCPS e id (const e)

-- usually a = Expr
simplifyCPS :: Expr -> (Expr -> a) -> (() -> a) -> a
simplifyCPS e changed same =
    case e of
      V _ -> same ()
      C _ -> same ()
      e1 :+: e2 -> simplifyCPS e1 (\e1' -> simplifyCPS e2 (\e2' -> simplPlusCPS e1' e2' changed (const (changed (e1' :+: e2'))))
                                                          (\()  -> simplPlusCPS e1' e2  changed (const (changed (e1' :+: e2)))))
                                  (\()  -> simplifyCPS e2 (\e2' -> simplPlusCPS e1  e2' changed (const (changed (e1  :+: e2'))))
                                                          (\()  -> simplPlusCPS e1  e2  changed same))

simplPlusCPS :: Expr -> Expr -> (Expr -> a) -> (() -> a) -> a
simplPlusCPS (C 0) e     changed same = changed e
simplPlusCPS e     (C 0) changed same = changed e
simplPlusCPS (C m) (C n) changed same = changed $ C (m + n)
simplPlusCPS _      _    changed same = same ()

----------------
--  Examples  --
----------------

l, r :: Expr
l = (C 0 :+: V 'c') :+: (C 10 :+: C 32)
r = (V 'c' :+: (C 0 :+: C 0))

simplifyMe :: Expr
simplifyMe = (C 0 :+: V 'c') :+: (C 10 :+: C 32) :+: (V 'c' :+: (C 0 :+: C 0))


----------------------------------------------
-- Only tests below - can be safely ignored --
----------------------------------------------

------------------------------------
--  An instance for quickCheck.   --
------------------------------------

instance Arbitrary Expr where
  arbitrary = do
    n <- choose (0,2) :: Gen Int
    case n of
      0 -> 
        V <$> elements ['a', 'b']
      1 -> 
        C <$> elements [0, 1]
      2 ->
        (:+:) <$> arbitrary <*> arbitrary

------------------
--  Properties  --
------------------

simplEval :: Expr -> Bool
simplEval e = eval e (const 1) == eval (simplify e) (const 1)

simplEquiv :: Expr -> Bool
simplEquiv e = simplify e == simplify2 e

-----------------
--  Run tests  --
-----------------

runTests :: IO ()
runTests = quickCheck simplEquiv