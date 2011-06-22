use "generalBacktracking.sml" ;

structure AnyMonoid : MONOID_PLUS
  = struct
  type 'a answer = 'a option

  val zero = NONE

  fun combine  NONE    m = m
    | combine (SOME a) m = SOME a

  fun pure a = SOME a
end

datatype regexp = Lit of char 
                | And of regexp * regexp 
                | Alt of regexp * regexp

(* bez sensu, chcemy a = regexp oraz answer = char list option ... *)
(*
structure Back = Backtracking (structure m = AnyMonoid)(type a = char list)

fun isSome (SOME a) = true
  | isSome NONE     = false

fun matches r chars = 
    let open Back in
        let
            fun iter (Lit char) []    = NONE
              | iter (Lit char) (first::rest) =
                if char = first then
                    SOME rest
                else
                    NONE
              | iter (And (r1, r2)) l =
                (case iter r1 l of
                     NONE => NONE
                   | SOME xs => iter r2 xs)
              | iter (Alt (r1, r2)) l = 
                withBacktracking (fn () => 
                                     
        in
            case iter r chars of
                SOME [] => true
              | _       => false
        end
    end
                                                 

*)

fun matches_cps r chars ok fail =
    case r of
        Lit c => (case chars of
                      [] => fail ()
                    | (h :: t) =>
                      if h = c then
                          ok t
                      else
                          fail ())
      | And (r1, r2) => matches_cps r1 chars 
                         (fn chars' => matches_cps r2 chars' ok fail)
                          fail
      | Alt (r1, r2) => matches_cps r1 chars
                        (fn chars' => matches_cps r2 chars
                                      (fn chars'' => ok chars'')
                                      (fn ()      => ok chars'))
                        (fn () => matches_cps r2 chars ok fail)
                                       


fun matches r chars = matches_cps r chars List.null (fn x => false)


