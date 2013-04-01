
(*  JSBack.sml : translation of lambda terms to lists of JS-instructions. *)

open Const Lambda Prim JSInstruct;


(* The translator from lambda terms to lists of instructions.

   env : the map from Lvar ids to stackptr offsets; updated by side-effects
   staticfail : the pair (label,sz) where Lstaticfail must branch.

   The tests on the continuation detect tail-calls and avoid jumps to
   jumps, or jumps to function returns.

*)

fun compileSCon scon =
  case scon of
    INTscon i => JSNUMscon Int.toString i
  | WORDscon w => JSNUMscon (Int.toString o Word.toInt) w
  | CHARscon c => JSSTRscon Char.toString c
  | REALscon r => JSNUMscon Real.toString r
  | STRINGscon s => JSSTRscon s
;

fun compileLambda exp =
  case exp of
    Lconst (ATOMsc scon) => compileSCon scon
  | Lprim (Pccal call, args) => compileCall call args
  | Lprim (Pget_global uid, _) => JSGetVar uid
  | Lprim (Pset_global uid, [arg]) => JSSetVar (uid, compileLambda arg)

  and compileCall (name, arity) args =
    case (name, args) of
      ("sml_concat", arg1 :: arg2 :: []) =>JSAdd (compileLambda arg1, compileLambda arg2)
;