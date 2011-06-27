 use "shift.sml" ;

structure Ctrl = Shift_and_Reset (type answer = int list list) ;
open Ctrl ;
fun emit v = shift (fn k => v :: k [])

fun suf_naive [] = [[]]
  | suf_naive (l as (x :: xs)) = l :: suf_naive xs

fun suf xs = 
    let
        fun iter [] k = k [[]]
          | iter (l as (x :: xs)) k = iter xs (fn yss => k (l :: yss))
    in
        iter xs (fn x => x)
    end

fun suf_sr xs =
    let
        fun iter []             = emit []
          | iter (l as (x::xs)) = (emit l ; iter xs)
    in
        reset (fn () => iter xs)
    end

(* prefixes *)

(* v2 *)
fun pref xs = 
    let 
        fun iter []        f = [ f [] ]
          | iter (x :: xs) f = f [] :: iter xs (fn l => f (x :: l))
    in
        iter xs (fn x => x)
    end

(* v2 - double dlist or dlist + cps *)
fun pref_cps xs =
    let
        fun iter []        f k = k [ f [] ]
          | iter (x :: xs) f k = iter xs (fn l  => f (x :: l)) 
                                         (fn ls => k (f [] :: ls))
    in
        iter xs (fn x => x) (fn x => x)
    end

fun snoc x diff_xs = (fn ls => diff_xs (x :: ls))

(* v2 - diff lists for buildPrefix, s/r for building the result list *)

fun pref_sr xs =
    let
        fun iter []        buildPref = emit (buildPref [])
          | iter (x :: xs) buildPref = (emit (buildPref []) ; iter xs (snoc x buildPref))
    in
        reset (fn () => iter xs (fn x => x))
    end

(* v1 = dlist + stack acc *)
fun pref_acc xs = 
    let 
        fun iter []        f acc = f [] :: acc
          | iter (x :: xs) f acc = iter xs (fn l => f (x :: l)) (f [] :: acc)
    in
        iter xs (fn x => x) []
    end
