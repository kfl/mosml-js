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

(* Handle constants of lambda primitives*)
fun compileSCon scon =
let
  val const = case scon of
    INTscon i => JSINTscon (Int.toString i)
  | WORDscon w => JSWORDscon (Word.toString w)
  | CHARscon c => JSSTRscon (Char.toString c)
  | REALscon r => JSREALscon (Real.toString r)
  | STRINGscon s => JSSTRscon s
in
  JSATOMsc(const)
end;

(* Handle constants *)
fun compileSC sconst = 
  case sconst of
    ATOMsc(scon) => compileSCon scon
  | BLOCKsc(CONtag(tag, span), sclist) => 
    (case (tag, span, sclist) of
      (0,1,sclist)      => JSLISTsc(map (fn x => JSConst(x)) (map compileSC sclist))
    | (0,2,_)           => JSBoolsc(JSFalse)
    | (1,2,[])          => JSBoolsc(JSTrue)
    | (1,2,[elem,list]) => JSLISTsc(JSConst(compileSC elem)::(compileBlock list))
    ) (* Will not catch other tags/spans, like refs. *)
 (* | QUOTEsc(_) => () *)
(* Handle nested list structure (flat it) *)
and compileBlock block = 
  case block of
      BLOCKsc(CONtag(0,2),_)           => []
    | BLOCKsc(CONtag(1,2),[elem,list]) => JSConst(compileSC elem)::(compileBlock list)
    | _                                => []

(* Converts expressions of Lambda to expressions of abstract JS.
   exp is the lambda expression.
   env is the current scope environment.
   returns expression of abstract JS. *)
fun compileJSLambda (exp : Lambda) env =
let
  val env' = updateEnv env
in
  case exp of
    Lapply (func, args) => JSApply(compileJSLambda func env, compileJSLambdaList args env)
  | Lconst (sconst) => JSConst (compileSC sconst)
  | Lfn (exp) => JSFun (compileJSLambda exp env', hd(env'))
  | Lif (tst, exp1, exp2) => JSIf (compileJSLambda tst env, compileJSLambda exp1 env, compileJSLambda exp2 env)
  | Llet ([exp1], exp2) =>
        JSScope (extractLetList exp2 [JSSetVar(hd(env'), compileJSLambda exp1 env)] env')
  | Lletrec ([exp1], exp2) => 
        JSScope (extractLetList exp2 [JSSetVar(hd(env'), compileJSLambda exp1 env')] env')
  | Lprim (prim, args) => compileJSPrim prim args env
  | Lvar (i) => JSGetVar(nth(env,i))
  | Lseq (exp1, exp2) => JSSeq(compileJSLambda exp1 env, compileJSLambda exp2 env)
  | Landalso (exp1, exp2) => JSAnd(compileJSLambda exp1 env, compileJSLambda exp2 env)
  | Lorelse (exp1, exp2) => JSOr(compileJSLambda exp1 env, compileJSLambda exp2 env)
  | Lwhile (exp, body) => JSWhile(compileJSLambda exp env, compileJSLambda body env)
  | Lunspec => JSUnspec
  | Lstatichandle (Lcase (exp, clist), def) => JSSwitch(compileJSLambda exp env, map (fn (scon,exp') => (JSConst(compileSCon scon), compileJSLambda exp' env)) clist , compileJSLambda def env)
  | _ => JSError(0) (* else print error *)
end

and compileJSPrim (prim : primitive) args env =
  case (prim, args) of
    (Psmladdint, [arg1, arg2]) => JSAdd (JSAddInt, compileJSLambda arg1 env, compileJSLambda arg2 env)
  | (Psmlsubint, [arg1, arg2]) => JSSub (JSSubInt, compileJSLambda arg1 env, compileJSLambda arg2 env)
  | (Psmlmulint, [arg1, arg2]) => JSMul (JSMulInt, compileJSLambda arg1 env, compileJSLambda arg2 env)
  | (Psmldivint, [arg1, arg2]) => JSDiv (JSDivInt, compileJSLambda arg1 env, compileJSLambda arg2 env)
  | (Psmlmodint, [arg1, arg2]) => JSMod (JSModInt, compileJSLambda arg1 env, compileJSLambda arg2 env)
  | (Pccall call, args) => compileCall call args env
  | (Pget_global(uid,_), _ )=> JSGetVar uid
  | (Pset_global(uid,_), [arg]) => JSSetVar (uid, compileJSLambda arg env)
  | (Pfield(i), [Lvar(j)]) => JSGetList(Int.toString(i),(nth(env,j)))
  | (Ptest(bool_test), [arg1, arg2]) => (* do not work on lists *)
    (case bool_test of
      Pint_test(PTeq) => JSTest(JSeq, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | _ => JSError(0)
    )
  | (Pnot, [arg]) => JSNot(compileJSLambda arg env)
  | (Pmakeblock(block),args) => JSConst(JSLISTsc(compileList block args env))

  | _ => JSError(0) (* else print error *)

and compileJSLambdaList [] _ = []
  | compileJSLambdaList (exp::exps) env = (compileJSLambda exp env)::(compileJSLambdaList exps env)

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
    ("sml_concat", arg1 :: arg2 :: []) => JSAdd (JSConcat, compileJSLambda arg1 env, compileJSLambda arg2 env)
  | _ => JSError(0) (* else do nothing *)

and compileList block list env = 
  case (block, list) of 
    (CONtag(0,1), _) => map (fn x => compileJSLambda x env) list
  | (CONtag(1,2), [arg, Lprim(Pmakeblock(block1),args)]) => compileJSLambda arg env::(compileList block1 args env)
  | (CONtag(1,2), [arg, Lconst(block1)]) => compileJSLambda arg env::(compileBlock block1)
;