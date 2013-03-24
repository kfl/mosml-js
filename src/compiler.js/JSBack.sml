(*  JSBack.sml : translation of lambda terms to lists of JS-instructions. *)

open List Fnlib Mixture Const Lambda Prim Instruct;


(* The translator from lambda terms to lists of instructions.

   env : the map from Lvar ids to stackptr offsets; updated by side-effects
   staticfail : the pair (label,sz) where Lstaticfail must branch.

   The tests on the continuation detect tail-calls and avoid jumps to
   jumps, or jumps to function returns.

*)

fun compileExp env staticfail =

fun compileLambda exp =
  case exp of
    Lconst (ATOMsc scon) =>
  | Lprim (prim, args)
  ;