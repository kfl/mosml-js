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
  JSConst(const)
end;

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
  | Lconst scon => compileConst scon
  | Lfn (exp) => JSFun(compileJSLambda exp env', hd(env'))
  | Lif (tst, exp1, exp2) =>
      JSIf (compileJSLambda tst env, compileJSLambda exp1 env, compileJSLambda exp2 env)
  | Llet ([exp1], exp2) =>
        JSScope(extractLetList exp2 [JSSetVar(hd(env'), compileJSLambda exp1 env)] env')
  | Lletrec (args, exp2) => 
      let
        val l = (length args)
        fun updEnv 0 e = e
          | updEnv x e = updEnv (x-1) (updateEnv e)
        val env'' = updEnv l env
        
        fun compileLetrecList [] _ list = list
          | compileLetrecList (arg::args) n list = compileLetrecList args (n-1) ((JSSetVar(nth(env'', n), compileJSLambda arg env''))::list)
        val list' = compileLetrecList args (l-1) []
      in
        JSScope(extractLetList exp2 list' env'')
      end
  | Lprim (prim, args) => compileJSPrim prim args env
  | Lvar (i) => JSGetVar(nth(env,i))
  | Lseq (exp1, exp2) =>
    (case exp1 of
      (Lprim(Pset_global(_,_),_))=> JSSeqFun(compileJSLambda exp1 env, compileJSLambda exp2 env)
    | _ => JSSeq(compileJSLambda exp1 env, compileJSLambda exp2 env))
  | Landalso (exp1, exp2) => JSAnd(compileJSLambda exp1 env, compileJSLambda exp2 env)
  | Lorelse (exp1, exp2) => JSOr(compileJSLambda exp1 env, compileJSLambda exp2 env)
  | Lwhile (exp, body) => JSWhile(compileJSLambda exp env, compileJSLambda body env)
  | Lunspec => JSUnspec
  | Lstatichandle (Lcase (exp, clist), def) =>
      JSSwitch(0, compileJSLambda exp env,
      map (fn (scon,exp') => (compileSCon scon, compileJSLambda exp' env)) clist,
      compileJSLambda def env)
  | Lstatichandle (Lswitch (_, exp, clist), def) =>
      JSSwitch(1, compileJSLambda exp env,
      map (fn (CONtag (tag,span), exp') => (JSConst(JSINTscon (Int.toString tag )),
      compileJSLambda exp' env)) clist, compileJSLambda def env)
  | Lshared (lref, _) => compileJSLambda (!lref) env
  | Lhandle (exp1, (Lif(Lprim(Ptest(Peq_test), [arg1 as (Lprim(_,[Lvar(j)])), arg2]), exp2, _))) =>
      JSTryCatch(compileJSLambda exp1 env, JSGetVar(nth(env',j)), compileJSLambda arg1 env', compileJSLambda arg2 env', compileJSLambda exp2 env')
  | _ => JSError("compileJSLambda") (* else print error *)
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
  | (Pfield(i), [arg]) => (JSGetField(compileGetField arg [Int.toString(i)] env) handle Subscript => JSError("Pfield"))
  | (Ptest(bool_test), [arg1, arg2]) =>
    (case bool_test of
      Pint_test(PTeq)                => JSTest(JSeq, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | Peq_test                       => JSTest(JSeq, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | Pnoteq_test                    => JSTest(JSneq, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | Pnoteqtag_test(CONtag(tag, _)) => JSTest(JSneqtag(tag), compileJSLambda arg1 env, compileJSLambda arg2 env)
      (* maybe not to be used, fatal error in back.sml *)
    | _ => JSError("Ptest")
    )
  | (Pnot, [arg]) => JSNot(compileJSLambda arg env)
  | (Pmakeblock(CONtag(tag,_)),args) => compileBlock tag args env
  | (Praise, [arg]) => JSRaise(compileJSLambda arg env)
  | (Praise, args) => JSError("Praise") (* TODO handle more args? Cases? *)
  | _ => JSError("compileJSPrim") (* else print error *)

and compileJSLambdaList [] _ = []
  | compileJSLambdaList (exp::exps) env = (compileJSLambda exp env)::(compileJSLambdaList exps env)

and extractLetList exp list env =
let
  val env' = updateEnv env
in
  case exp of
    Llet([exp1], exp2) => extractLetList exp2 (JSSetVar(hd(env'), compileJSLambda exp1 env)::list) env'
  | Lletrec(args, exp2) => 
    let
      val l = (length args)
      fun updEnv 0 e = e
        | updEnv x e = updEnv (x-1) (updateEnv e)
      val env'' = updEnv l env
      fun compileLetrecList [] _ list = list
        | compileLetrecList (arg::args) n list = compileLetrecList args (n-1) ((JSSetVar(nth(env'', n), compileJSLambda arg env''))::list)
      val list' = compileLetrecList args (l-1) []
    in
      extractLetList exp2 list' env''
    end
  | _ => (rev list,compileJSLambda exp env)
end

and compileCall (name, arity) args env =
  case (name, args) of
    ("sml_concat", arg1 :: arg2 :: []) => JSAdd (JSConcat, compileJSLambda arg1 env, compileJSLambda arg2 env)
  | _ => JSError("compileCall") (* else do nothing *)

and compileBlock tag list env =
  JSBlock(tag, map (fn x => compileJSLambda x env) list)

and compileBlocksc tag sclist =
  JSBlock(tag, map (fn x => compileConst x) sclist)

and compileConst (ATOMsc(scon)) = compileSCon scon
  | compileConst (BLOCKsc(CONtag(tag,_), args)) = compileBlocksc tag args

and compileGetField (Lprim(Pget_global(uid,_),_)) idxs _ = (idxs, uid)
  | compileGetField (Lvar(j)) idxs env = (idxs, nth(env,j))
  | compileGetField (Lprim(Pfield(i),[arg])) idxs env = compileGetField arg (Int.toString(i)::idxs) env
;