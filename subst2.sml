Control.Print.printDepth := 100;

(* This time we will analyze the representation of lambda-terms from
   the locally nameless idea *)

type name = string

datatype term = B of int
              | F of name
              | Abs of term
              | App of term * term

(* Again we will look at the substitution operation. We will be substituting a given
  term s for all occurences of the given name v in the given term t.

  Also again, in all version but the naive one, we will want to avoid rebuilding the
  input term and (this is a new aspect in comparison with the previous file) we will
  strive for as much sharing with the input structure as possible (using the same 
  techniques.
*)

fun substFree_naive t v s = 
    let 
        fun iter (B n)          = B n
          | iter (F x)          = if x = v then s else F x
          | iter (Abs t0)       = Abs (iter t0)
          | iter (App (t1, t2)) = App (iter t1, iter t2)
    in
        iter t
end

val ex1 = App (Abs (F "x"), App (F "y" , B 0))

(* We notice, that the presence of a binary constructor introduces a new
   possibility: i.e. the left subterm may have to be shared, while the right
   one will change.
   The only situation, in which the whole App (t1, t2) term will be shared is when
   both subterms won't change. In other words, a change in *either* subterm makes
   it necessary to allocate a new App cell.
*)

(* The most straightforward version seems to be the one using Maybe.
   The code doesn't use any higher-order functions or operators on 
   maybe to make the data flow easy to follow. See the Haskell rewrite
   below for a less repetitive version. *)

fun substFree_option_monad t v s = 
    let 
        fun iter (B _) = NONE
          | iter (F x) = if x = v then
                             SOME s
                         else
                             NONE
          | iter (Abs t0) = 
            (case iter t0 of
                SOME t0' => SOME (Abs t0')
              | NONE     => NONE)
          | iter (App (t1, t2)) =
            (case (iter t1, iter t2) of
                 (SOME t1' , SOME t2') => SOME (App (t1', t2'))
               | (SOME t1' , NONE    ) => SOME (App (t1', t2 ))
               | (NONE     , SOME t2') => SOME (App (t1 , t2'))
               | (NONE     , NONE    ) => NONE)
    in
        case iter t of
            SOME t' => t'
          | NONE    => t
    end

(* We see that indeed, we save one App cell in only one case.
   However, we save a lot of memory - we are able to perform pretty
   aggressive sharing! *)
  

(* Again we present an idiomatic Haskell version that is perhaps more
   elegant and shorter, but is slightly more criptic and it's
   more difficult to see at first sight how much sharing do we get *)

(* Haskell:

import Data.Maybe

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
*)

(* In the previous file we started with an exception-based solution.
   This time, however, we might be out of luck. It is of course possible to
   erase Justs, replace every Nothing with a raise and pattern matching with 
   a handler, but it's difficult to find a way to compose the handlers nicely.

*)

exception NoChange

fun substFree_exception t v s =
    let 
        fun iter (B _) = raise NoChange
          | iter (F x) = if x = v then 
                             s 
                         else 
                             raise NoChange
          | iter (Abs t0)       = Abs (iter t0)
          | iter (App (t1, t2)) = s
            (* Wrong: App (iter t1, iter t2) *)
            
    in
        iter t handle NoChange => t
    end

(* In the 'Wrong:' comment we see a problem. If traversing only one subterms
   raises an exception we return to the top, even though we should have rebuild
   one branch! Imitating the pattern matching seems inevitable.
*)



val all_functions = [ substFree_naive , substFree_option_monad ,
                      substFree_exception ]

fun test_all t v s = List.map (fn f => f t v s) all_functions
