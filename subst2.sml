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

val ex1 = App (Abs (F "x"), Abs (App (F "y" , B 0)))
val ex2 = Abs (F "y")

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
          | iter (App (t1, t2)) = 
              (* 1: *)  
                     (* App (iter t1, iter t2) *)
              (* 2: *)
                     (* App (iter t1 handle NoChange => t1,
                             iter t2 handle NoChange => t2) *)
            (let 
                val s1 = iter t1 
            in 
                (* there was a change, we have to rebuild *)
                App (s1, iter t2 handle NoChange => t2)
            end) 
            (* rebuild only iff iter t2 is rebuilt *)
            handle NoChange => App (t1, iter t2) 
    in
        iter t handle NoChange => t
    end

(* We first consider a few failed approaches:

   1) we don't handle any exceptions in the app case. 
     this leads to a wrong result when exactly one subterm changes, because
     we signal that nothing has changed at all. 

   2) when we handle each case separately we obtain a function that is correct
      wrt. to the result value, but it doesn't meet our objection of achieving
      maximal sharing. It's not that bad, though. We miss the sharing opportunity
      only in the case when neither of branches has changed.
*)

(* The correct version:
   we bind the result of the substitution in the left branch.
   If there is a change there, no uncaught exception is raised.
   We know that we need to allocate the App no matter what happens in the
   right subtree, so we just do it while using the handle construct to pick
   iter t2 or t2, whichever proves to be correct.
   If iter t1 raises the NoChange exception we get into the outer handler.
   Now is iter t2 finished with no exceptions the right subtree has changed,
   and allocation of App is justified, while if the exception IS thrown, then
   it will propagate and App won't be allocated.

*)

(* Please notice, that 
     fromMaybe t1 (iter t1) 
   and 
     iter t1 handle NoChange => t1
   are intentionally identical here!
*)


(* Time for CPS! *)

fun substFree_cps t v s = 
    let 
        fun iter (B _) same = (fn changedK => same ())
          | iter (F x) same = (fn changedK =>
                                  if x = v then
                                      changedK s
                                  else
                                      same ())

          | iter (Abs t0) same = (fn changedK => 
                                     iter t0 same 
                                             (fn t0' => changedK (Abs t0')))

          | iter (App (t1, t2)) same = 
            (fn changedK => 
                iter t1 (fn ()  => iter t2 same
                                           (fn t2' => changedK (App (t1 , t2'))))
                        (fn t1' => iter t2 (fn ()  => changedK (App (t1', t2)))
                                           (fn t2' => changedK (App (t1', t2')))))
    in
        iter t (fn () => t) (fn x => x)
    end

(* The cps version uses two continuations same and changedK.
   changedK is invoked when the term has changed, while same is
   used for singnaling that nothing has changed. Because same is a 
   function of type unit -> Term, we can also view it as a delayed computation
   that will yield a Term.

   In the App case we have to stack the continuation in such a way, that
   changedK is called when at least one subterm has changed.

   Open question: can we write this function cleanly using only one continuation?
*)

(* Can we implement this function using shift/reset? shift0/reset0? shift2/reset2? *)

;
use "shift.sml" ;
structure TermCtrl = Shift_and_Reset (type answer = term) ;

fun substFree_shift_reset tt v s = 
    let open TermCtrl in
        let
            fun iter t same =
                case t of
                    B n => 
                  | F x => if x = v then
                               
                           else
                               
                  | Abs t0       => 
                  | App (t1, t2) => 
                      
                      
        in
            reset (fn () => iter tt (fun () => tt))
        end
    end




val all_functions = [ substFree_naive 
                    , substFree_option_monad
                    , substFree_exception 
                    , substFree_cps 
                    , substFree_shift_reset
                    ]

fun test_all t v s = List.map (fn f => f t v s) all_functions
