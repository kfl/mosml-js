(* JSBack.sml : translation of lambda terms to lists of intermediate JS-instructions. *)

open Const Lambda Prim JSInstruct List;

val varCount = ref 0;
fun updateEnv env = 
let 
  val var = "var_"^Int.toString(!varCount);
  val qualid : QualifiedIdent = {id=[var] , qual=""};
in
  (
  varCount := !varCount+1;
  qualid::env
  )
end;

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

fun compileJSLambda (exp : Lambda) env =
let
  val env' = updateEnv env
in
  case exp of
    Lconst (ATOMsc scon) => JSConst (compileSCon scon)
  | Lfn (exp) => JSFun (compileJSLambda exp env', hd(env'))
  | Lprim (Psmladdint, [arg1, arg2]) => JSAdd (compileJSLambda arg1 env, compileJSLambda arg2 env)
  | Lprim (Pccall call, args) => compileCall call args env
  | Lprim (Pget_global(uid,_), _) => JSGetVar uid
  | Lprim (Pset_global(uid,_), [arg]) => JSSetVar (uid, compileJSLambda arg env)
  | Llet ([exp1], exp2) =>
        JSScope (extractLetList exp2 [JSSetVar(hd(env'), compileJSLambda exp1 env)] env')
  | Lletrec ([exp1], exp2) => 
        JSScope (extractLetList exp2 [JSSetVar(hd(env'), compileJSLambda exp1 env')] env')
  | Lvar (i) => JSGetVar(nth(env,i))
  | _ => JSError(0) (* else print error *)
end

and extractLetList exp list env =
let
  val env' = updateEnv env
in
  case exp of
    Llet([exp1], exp2) => extractLetList exp2 (JSSetVar(hd(env'), compileJSLambda exp1 env)::list) env'
  | Lletrec([exp1], exp2) => extractLetList exp2 (JSSetVar(hd(env'), compileJSLambda exp1 env')::list) env'
  | _ => (rev list,compileJSLambda exp env)
end

and compileCall (name, arity) args env =
  case (name, args) of
    ("sml_concat", arg1 :: arg2 :: []) => JSAdd (compileJSLambda arg1 env, compileJSLambda arg2 env)
  | _ => JSError(0) (* else do nothing *)
;