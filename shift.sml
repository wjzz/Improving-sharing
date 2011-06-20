(* shift-and-reset.sml *)
(* was: *)
(* ctrl.sml -*- sml -*- *)
(* from Andrzej Filinski's article at POPL'94 *)

(* ********** *)

signature ESCAPE
= sig
    type void
    val coerce : void -> 'a
    val escape : (('a -> void) -> 'a) -> 'a
  end

structure Escape : ESCAPE
= struct
    datatype void = VOID of void
    fun coerce (VOID v) = coerce v
    fun escape f
        = SMLofNJ.Cont.callcc (fn k => f (fn x => SMLofNJ.Cont.throw k x))
  end

signature SHIFT_AND_RESET
= sig
    type answer
    val shift : (('a -> answer) -> answer) -> 'a
    val reset : (unit -> answer) -> answer
  end

functor Shift_and_Reset (type answer) : SHIFT_AND_RESET
= struct
    open Escape
    exception MissingReset
    val mk : (answer -> void) ref = ref (fn _ => raise MissingReset)
    fun abort x = coerce (!mk x) 
    type answer = answer
    fun reset t
        = escape (fn k => let val m = !mk 
                          in mk := (fn r => (mk := m; k r)); 
                             abort (t ())
                          end)
    fun shift h
        = escape (fn k => abort (h (fn v => (reset (fn () => coerce (k v))))))
  end;

(* ********** *)

(* eof *)