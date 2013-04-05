(* JSBack.sml : translation of lambda terms to lists of intermediate JS-instructions. *)

open Const Lambda Prim JSInstruct;

fun compileSCon scon =
let
  val const = case scon of
    INTscon i => JSNUMscon (Int.toString i)
  | WORDscon w => JSNUMscon (Int.toString (Word.toInt w))
  | CHARscon c => JSSTRscon (Char.toString c)
  | REALscon r => JSNUMscon (Real.toString r)
  | STRINGscon s => JSSTRscon s
in
  JSATOMsc(const)
end;

fun compileJSLambda exp =
  case exp of
    Lconst (ATOMsc scon) => JSConst (compileSCon scon)
  | Lprim (Pccall call, args) => compileCall call args
  | Lprim (Pget_global(uid,_), _) => JSGetVar uid
  | Lprim (Pset_global(uid,_), [Lfn (arg)]) => JSFun (uid, compileJSLambda arg)
  | Lprim (Pset_global(uid,_), [arg]) => JSSetVar (uid, compileJSLambda arg)
  | _ => JSError(0) (* else do nothing *)

  and compileCall (name, arity) args =
    case (name, args) of
      ("sml_concat", arg1 :: arg2 :: []) =>JSAdd (compileJSLambda arg1, compileJSLambda arg2)
    | _ => JSError(0) (* else do nothing *)
;