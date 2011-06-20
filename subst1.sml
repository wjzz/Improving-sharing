Control.Print.printDepth := 100;

(* We begin with a trivial example. We will experiment with more and more 
   compicated examples in other files. *)

datatype Term = A | B | T of Term

(* We will deal will substituting a given term for the A leaf. *)

(* Substitute s for A, if found. Otherwise do not change anything. *)

fun substForA_naive t s =
    let 
        fun iter A = s
          | iter B = B
          | iter (T t0) = T (iter t0)
    in
        iter t
    end

val exA = T (T (T A))
val exB = T (T (T B))

(* The bad thing about substForA is that if no A is found (so the term is T (T (T ... B))),
   then the whole term is rebuild from scratch, even though we do not change anything at all!
   This is big waste of memory. 
                        
   We want to write a version that will rebuild the term if and only if it's required.
   We will consider several approaches based on different ways to handle control.                                                                        
*)

(* Since we know whether we need to allocate new cells or not at the very bottom of the 
   term, we can use exceptions: *)

exception NoChange                                                                                    

fun substForA_exceptions t s =
    let
        fun iter A      = s
          | iter B      = raise NoChange
          | iter (T t0) = T (iter t0)
    in
        iter t handle NoChange => t
    end

(* We see that this approach is pretty clear - we only had to adjust the base case. 
   Since SML is strict, no T's will be allocated is we end up in the B case.
*)

(* The next version is based on an idea mentioned in ezyang's blog:
     http://blog.ezyang.com/2011/06/a-pattern-for-increasing-sharing/

   The trick is to keep track of any changes explictly by using the Maybe monad. 
   We can use a dedicated datatype:
      data ChangeTracking a = Old | New a (or Same | Changed a)
   or just use the Maybe (Option) type.

   If we return Old we are stating that we don't need (so we don't want) to
   alter the given Term. If we return New t we state that the subterm has changed                       and the whole path from the top needs to be rebuild (since we are in the immutable world).        
                                                                                                        This approach is much heavier than the exception way, but it will scale better when we'll
   start analyzing more complicated terms (i.e. with branches/application node) and 
   functions on them.
*)

fun substForA_option_monad t s =
    let 
        fun iter A = SOME s
          | iter B = NONE
          | iter (T t0) = 
            case iter t0 of
                SOME t' => SOME (T t')
              | NONE    => NONE
    in
        case iter t of
            SOME t' => t'
          | NONE    => t
    end

(* The equivalent Haskell version is more succinct and uses
   two extremely common library functions for Maybe. *)

(* Haskell:

import Data.Maybe

subst :: Term -> Term -> Term
subst t s = fromMaybe t (iter t) where

  iter :: Term -> Maybe Term
  iter A      = Just s
  iter B      = Nothing
  iter (T t0) = T `fmap` iter t0

*)
                    
(* The next step is to reify the control via an explicit continuation and
   use the CPS-style. *)

fun substForA_cps t s = 
    let
        fun iter A      = (fn changedK => changedK s)
          | iter B      = (fn changedK => t)
          | iter (T t0) = (fn changedK => iter t0 (fn t' => changedK (T t')))
    in
        iter t (fn x => x)
    end ;

(* We can go back to direct style using delimited continuations *)

use "shift.sml" ;
structure TermCtrl = Shift_and_Reset (type answer = Term) ;

fun substForA_shift_reset t s =
    let 
        open TermCtrl 
    in
        let
            fun iter A      = s
              | iter B      = shift (fn k => t) (* abort *)
              | iter (T t0) = T (iter t0)
        in
            reset (fn () => iter t)
        end
    end

(* We observe that this version is identical in shape with the expression version,
  with abort beign raise and reset being handle! *)

(* Can we go from substForA_shift_reset to substForA_cps by the cps translation? *)

val all_functions = [ substForA_naive , substForA_exceptions , substForA_option_monad ,
                      substForA_cps   , substForA_shift_reset ]

fun test_all t s = List.map (fn f => f t s) all_functions
