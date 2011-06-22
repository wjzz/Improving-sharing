use "shift.sml" ;

datatype regexp = Empty
                | Lit  of int
                | And  of regexp * regexp 
                | Alt  of regexp * regexp
                | Star of regexp


fun matches_cps r chars ok fail combine =
    case r of
        Empty => ok chars
      | Lit c => (case chars of
                      [] => fail ()
                    | (h :: t) =>
                      if h = c then
                          ok t
                      else
                          fail ())
      | And (r1, r2) => matches_cps r1 chars 
                         (fn chars' => matches_cps r2 chars' ok fail combine)
                          fail
                          combine
      | Alt (r1, r2) => 
                       (* (combine (matches_cps r1 chars ok fail combine)
                                 (matches_cps r2 chars ok fail combine))
*)
(*        matches_cps r1 chars
                        (fn chars' => combine (ok chars') 
                                              (matches_cps r2 chars ok fail combine))
                        (fn () => matches_cps r2 chars ok fail combine)
                        combine
*)
        matches_cps r1 chars
                        (fn chars' => matches_cps r2 chars
                                      (fn chars'' => combine (ok chars')(ok chars''))
                                      (fn ()      => ok chars')
                                      combine)
                        (fn () => matches_cps r2 chars ok fail combine)
                        combine 
      | Star r => matches_cps (Alt (Empty, And (r, Star r))) chars ok fail combine
                                       


fun matches r chars = matches_cps r chars List.null (fn x => false) (fn x => fn y => x orelse y)
fun count_matches r chars = matches_cps r chars 
         (fn l => if List.null l then 1 else 0)  (fn x => 0) (fn x => fn y => x + y)

(* shift/reset based solution *)

structure Ctrl = Shift_and_Reset (type answer = bool)
open Ctrl

fun fail ()   = shift (fn k => false)
fun amb c1 c2 = shift (fn k => (reset (fn () => k (c1 ()))) orelse
                               (reset (fn () => k (c2 ()))))

fun emit a = a
;                   

fun matches_shift_reset r chars =
    case r of
        Empty => emit chars
      | Lit n => (case chars of
                     [] => fail ()
                   | h :: t => 
                     if h = n then emit t else fail ())
      | And (r1, r2) => matches_shift_reset r2 (matches_shift_reset r1 chars)
      | Alt (r1, r2) => amb (fn () => matches_shift_reset r1 chars) 
                            (fn () => matches_shift_reset r2 chars)
      | Star r => matches_shift_reset (Alt (Empty, And (r, Star r))) chars

;
fun matches2 r chars = reset (fn () => List.null (matches_shift_reset r chars))
