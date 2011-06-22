use "shift.sml" ;

signature BACKTRACKING 
  = sig
    type a
    type answer
    val emit    : a -> answer
    val amb     : a -> a -> a
    val ambList : a list -> a
    val fail    : unit -> answer
    val assert  : bool -> answer
    val withBacktracking : (unit -> answer) -> answer
end

signature MONOID_PLUS
  = sig
    type 'a answer
    val pure    : 'a -> 'a answer
    val zero    : 'a answer
    val combine : 'a answer -> 'a answer -> 'a answer
end

functor Backtracking (structure m : MONOID_PLUS) (type a) : BACKTRACKING
= struct
  type a = a
  type answer = a m.answer

  structure Ctrl = Shift_and_Reset (type answer = answer)
  open Ctrl
       
  val emit = m.pure

  fun amb c1 c2 = shift (fn k => m.combine (reset (fn () => k c1))
                                           (reset (fn () => k c2)))

  fun concat l = List.foldl (fn (x,y) => m.combine x y) m.zero l                  
  fun ambList l = shift (fn k => concat 
       (List.map (fn c => (reset (fn () => k c))) l))

  fun fail () = shift (fn k => m.zero)

  fun assert pred  = if pred then m.zero else fail ()

  fun withBacktracking comp = reset comp                
end

(* Example usage: efectful computations: *)

structure UnitMonoid : MONOID_PLUS
  = struct
  type 'a answer = unit
  val zero = ()
  fun combine c1 c2 = (c1 ; c2)
  fun pure x = ()
end

structure IntEfects = Backtracking (structure m = UnitMonoid) (type a = int)

fun printPairs l1 l2 =
    let
        open IntEfects
    in
        withBacktracking 
        (fn () => 
            let
                val a = ambList l1
                val b = ambList l2
                val _ = assert (a * b > 10)
            in
                print (Int.toString (a * b) ^ "\n")
            end)
    end
