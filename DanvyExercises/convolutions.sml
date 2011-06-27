fun convolutions xs ys = 
    let
        fun iter []      = (ys, [])
          | iter (x::xs) =
            let 
                val (z::zs, conv) = iter xs
            in
                (zs, (x,z) :: conv)
            end
    in
        #2 (iter xs)
    end

(* uses difference lists *)

fun convolutions_diff xs ys =
    let
        fun iter [] = (xs, (fn x => x))
          | iter (y :: ys) =
            let 
                val (z :: zs, acc) = iter ys
            in
                (zs, (fn tl => acc ((z,y) :: tl)))
            end
    in
        #2 (iter ys) []
    end

;
use "shift.sml" ;
structure Ctrl = Shift_and_Reset (type answer = (int * string) list) ;
open Ctrl ;
fun emit v = shift (fn k => v :: k []) ;

fun convolutions_sr xs ys = 
    let
        fun iter [] = xs
          | iter (y :: ys) = 
            let 
                val (x :: xs) = iter ys
            in
                (emit (x,y) ; xs)
            end
    in
        reset (fn () => (iter ys ; []))
    end

fun conv_pref xss yss = 
    let
        fun iter [] = (xss, (fn x => x), (fn x => [] :: x))
          | iter (y::ys) =
            let
                val (x :: xs, buildPref, insertEnd) = iter ys
            in
                (xs, (fn l => buildPref ((x,y) :: l)), (fn ll => insertEnd (buildPref [(x,y)] :: ll)))
            end
    in
        (#3 (iter yss)) []
    end

fun conv_pref_rev xss yss = 
    let
        fun iter [] = (xss, (fn x => x), [[]])
          | iter (y::ys) =
            let
                val (x :: xs, buildPref, acc) = iter ys
            in
                (xs, (fn l => buildPref ((x,y) :: l)), (buildPref [(x,y)]) :: acc)
            end
    in
        #3 (iter yss)
    end

fun conv_suff_lists xss yss = 
    let
        fun iter [] = (yss, [], [[]])
          | iter (x::xs) =
            let
                val (y :: ys, pref, acc) = iter xs
                val pref' = (x,y) :: pref
            in
                (ys, pref', pref' :: acc)
            end
    in
        #3 (iter xss)
    end

structure Ctrl2 = Shift_and_Reset (type answer = (int * string) list list) ;

fun conv_pref_sr xss yss =
    let        
        open Ctrl2
        fun emit v = shift (fn k => v :: k [])

        fun iter []        = (emit [] ; (xss, (fn x => x)))
          | iter (y :: ys) = 
            let
                val (x :: xs, prefs) = iter ys
                val prefs' = (fn l => prefs ((x,y) :: l))
            in
                (emit (prefs' []) ; (xs, prefs'))
            end
    in
        reset (fn () => (iter yss ; []))
    end
