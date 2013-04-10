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

fun compileJSLambda (exp : Lambda) =
  case exp of
    Lconst (ATOMsc scon) => JSConst (compileSCon scon)
  | Lfn (Llet([exp1], exp2)) =>  JSFun (extractLetList exp2 [compileJSLambda exp1])
  | Lfn (Lletrec([exp1], exp2)) =>  JSFun (extractLetList exp2 [compileJSLambda exp1])
  | Lfn (exp) =>  JSFun ([], compileJSLambda exp)
  | Lprim (Pccall call, args) => compileCall call args
  | Lprim (Pget_global(uid,_), _) => JSGetVar uid
  | Lprim (Pset_global(uid,_), [arg]) => JSSetVar (uid, compileJSLambda arg)
  | Llet ([exp1], exp2) => JSFun (extractLetList exp2 [compileJSLambda exp1])
  | Lletrec ([exp1], exp2) => JSFun (extractLetList exp2 [compileJSLambda exp1])
  | Lvar (i) => JSVar (i)
  | _ => JSError(0) (* else print error *)

and extractLetList exp list =
  case exp of
    Llet([exp1], exp2) => (extractLetList exp2 (compileJSLambda exp1::list))
  | Lletrec([exp1], exp2) => (extractLetList exp2 (compileJSLambda exp1::list))
  | _ => (list,compileJSLambda exp)

and compileCall (name, arity) args =
  case (name, args) of
    ("sml_concat", arg1 :: arg2 :: []) =>JSAdd (compileJSLambda arg1, compileJSLambda arg2)
  | _ => JSError(0) (* else do nothing *)
;