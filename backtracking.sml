use "shift.sml" ;

structure Ctrl = Shift_and_Reset (type answer = unit) ;
open Ctrl ;

fun amb c1 c2 = shift (fn k => (reset (fn () => k c1) ;
                               (reset (fn () => k c2))))
 
fun fail () = shift (fn k => ())

fun doList [] = ()
  | doList (x::xs) = (x ; doList xs)

fun ambList l = shift (fn k => doList (List.map (fn c => reset (fn () => k c)) l))

fun assert p = if p then () else fail () ;


fun hello () = reset (fn () => print (amb "x\n" "y\n"))

fun printAll l1 l2 = reset (fn () => 
                           let 
                               val a = ambList l1 
                               val b = ambList l2
                           in 
                               print (a ^ b ^ "\n")
                           end)

fun findEvenSums l1 l2 = 
    let 
        val results = ref []
    in
        reset (fn () =>
                  let
                      val a = ambList l1
                      val b = ambList l2
                  in
                      (assert ((a + b) mod 2 = 0) ;
                       results := (a,b) :: !results)
                  end) ;
        !results
    end
        
