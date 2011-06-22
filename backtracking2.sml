use "shift.sml" ;

structure Ctrl = Shift_and_Reset (type answer = int list) ;
open Ctrl ;

fun fail () = shift (fn k => [])

fun amb c1 c2 = shift (fn k => (reset (fn () => k c1)) @
                               (reset (fn () => k c2)))

fun ambList l = shift (fn k => 
   List.concat (List.map (fn x => reset (fn () => k x)) l))

fun assert pred = if pred then [] else fail ()
fun return a = [a]

fun hello () = reset (fn () => 
    let 
        val a1 = amb 1 2
        val a2 = amb 3 4
    in
        return (a1 + a2)
    end)

fun evenPairs l1 l2 = reset (fn () => 
  let
      val a1 = ambList l1
      val a2 = ambList l2
      val _  = assert ((a1 + a2) mod 3 = 0)
  in
      return (a1 + a2)
  end)

