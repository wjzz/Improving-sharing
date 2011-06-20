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
        fun iter (B n) = B n
          | iter (F x) = if x = v then 
                             s
                         else 
                             F x
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

(* The most straightforward version seems to be the one using Maybe: *)

fun substFree_option_monad t v s = 
    let 
        fun iter (B _) = NONE
          | iter (F x) = if x = v then
                             SOME e
                         else
                             NONE
          | iter (Abs t0) = 
            (case iter t0 of
                SOME t0' => SOME (Abs t0')
              | NONE     => NONE)
          | iter (App (t1, t2)) =
            (case (iter t1, iter t2) of
                 (SOME t1' , SOME t2') => SOME (App (t1', t2'))
               | (SOME t1' , NONE    ) => SOME (App (t1', t2))
               | (NONE     , SOME t2') => SOME (App (t1 , t2))
               | (NONE     , NONE    ) => NONE)
    in
        case iter t of
            SOME t' => t'
          | NONE    => t
    end


                        
val all_functions = [ substFree_naive ]

fun test_all t v s = List.map (fn f => f t v s) all_functions
