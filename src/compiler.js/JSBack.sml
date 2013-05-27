(* JSBack.sml : translation of lambda terms to lists of intermediate JS-instructions. *)

open Const Lambda Prim JSInstruct List;

(* Update the environment of the debruijn-index *)
val varCount = ref 0;
fun updateEnv env =
let
  val var = "$_var"^Int.toString(!varCount);
  val qualid : QualifiedIdent = {id=[var] , qual=""};
in
  (
  varCount := !varCount+1;
  qualid::env
  )
end;

(* Compile the test operator *)
val compileTest = fn
    PTeq    => JSeq
  | PTnoteq => JSneq
  | PTlt    => JSlt
  | PTle    => JSle
  | PTgt    => JSgt
  | PTge    => JSge

(* Compile the float operator *)
val compileFloat = fn
    Psmladdfloat => JSAddFloat
  | Psmlsubfloat => JSSubFloat
  | Psmlmulfloat => JSMulFloat
  | Psmldivfloat => JSDivFloat

(* Handle constants of lambda primitives*)
fun compileSCon scon =
let
  val const = case scon of
    INTscon i => if i < 0 then JSINTscon ("-"^Int.toString (Int.abs i)) else JSINTscon (Int.toString i)
  | WORDscon w => JSWORDscon ("0wx"^Word.toString w)
  | CHARscon c => JSSTRscon (Char.toString c)
  | REALscon r => if r < 0.0 then JSREALscon ("-"^Real.toString(Real.abs r)) else JSREALscon (Real.toString r)
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
  | Lfn (exp) => JSFun(compileJSLambda exp env', (hd(env'), ~1))
  | Lif (tst, exp1, exp2) =>
      JSIf (compileJSLambda tst env, compileJSLambda exp1 env, compileJSLambda exp2 env)
  | Llet (args, exp2) =>
      JSScope(extractLetList exp [] env)
  | Llet (args, exp2) => JSError("Llet")
  | Lletrec (args, exp2) =>
      JSScope(extractLetList exp [] env)
  | Lprim (prim, args) => compileJSPrim prim args env
  | Lvar (i) => JSGetVar(nth(env,i), ~1)
  | Lseq (exp1, exp2) =>
    (case exp1 of
      (Lprim(Pset_global _, _))=> JSSeqFun(compileJSLambda exp1 env, compileJSLambda exp2 env)
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
  | Lhandle (exp1, (Lif(Lprim(Ptest(Peq_test), [arg1 as (Lprim(_,[Lvar(j)])), arg2]), exp2, exp3)) ) =>
      JSTryCatch(compileJSLambda exp1 env, JSGetVar(nth(env',j), ~1), 
          compileJSLambda arg1 env', compileJSLambda arg2 env', 
          compileJSLambda exp2 env', compileJSLambda exp3 env')
  | _ => JSError("compileJSLambda") (* else print error *)
end

and compileJSPrim (prim : primitive) args env =
  case (prim, args) of
    (Psmlnegint, [arg])        => JSOperator(JSNegNum, [compileJSLambda arg env])
  | (Psmladdint, [arg1, arg2]) => JSOperator (JSAddInt, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Psmlsubint, [arg1, arg2]) => JSOperator (JSSubInt, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Psmlmulint, [arg1, arg2]) => JSOperator (JSMulInt, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Psmldivint, [arg1, arg2]) => JSOperator (JSDivInt, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Psmlmodint, [arg1, arg2]) => JSOperator (JSModInt, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Paddint, [arg1, arg2])    => JSOperator (JSAddWord, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Psubint, [arg1, arg2])    => JSOperator (JSSubWord, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Pmulint, [arg1, arg2])    => JSOperator (JSMulWord, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Pdivint, [arg1, arg2])    => JSOperator (JSDivWord, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Pmodint, [arg1, arg2])    => JSOperator (JSModWord, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Pandint, [arg1, arg2])    => JSOperator (JSAndWord, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Pfloatprim(Psmlnegfloat), [arg]) => JSOperator(JSNegNum, [compileJSLambda arg env])
  | (Pfloatprim(Pfloatofint), [arg]) => compileJSLambda arg env
  | (Pfloatprim(fprim), [arg1, arg2]) =>
      JSOperator(compileFloat fprim, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | (Pccall call, args) => compileCall call args env
  | (Pget_global (qualid as (ident, i)),_) => 
    (case #qual ident of 
      "General" => 
        JSGetVar ({id=(("$_mosmllib."^(hd(#id ident)))::(tl(#id ident))),qual=(#qual ident)},i)
    | _ => JSGetVar qualid
    )
  | (Pset_global qualid, [arg]) => JSSetVar (qualid, compileJSLambda arg env)
  | (Pfield(i), [arg]) => (JSGetField(compileGetField arg [Int.toString(i)] env) handle Subscript => JSError("Pfield"))
  | (Ptest(bool_test), [arg1, arg2]) =>
    (case bool_test of
      Peq_test           => JSTest(JSeq, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | Pnoteq_test        => JSTest(JSneq, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | Pint_test(tst)     => JSTest(compileTest tst, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | Pfloat_test(tst)   => JSTest(compileTest tst, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | Pstring_test(tst)  => JSTest(compileTest tst, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | Pword_test(tst)    => JSTest(compileTest tst, compileJSLambda arg1 env, compileJSLambda arg2 env)
    | _ => JSError("Ptest")
    )
  | (Pnot, [arg]) => JSNot(compileJSLambda arg env)
  | (Pmakeblock(CONtag(tag,_)),args) => compileBlock tag args env
  | (Praise, [arg]) => JSRaise(compileJSLambda arg env)
  | (Psetfield(i), [arg1, arg2]) => JSSetField(i, compileJSLambda arg1 env, compileJSLambda arg2 env)
  | (Pintoffloat, [arg]) => compileJSLambda arg env
  | (Pstringlength, [arg]) => JSOperator(JSStringLength, [compileJSLambda arg env])
  | _ => JSError("compileJSPrim") (* else print error *)

and compileJSLambdaList [] _ = []
  | compileJSLambdaList (exp::exps) env = (compileJSLambda exp env)::(compileJSLambdaList exps env)

and extractLetList exp list env =
let
  fun updEnv 0 e = e
    | updEnv x e = updEnv (x-1) (updateEnv e)
  fun compileLetrecList args =
    let
      val l = (length args)
      val env' = updEnv l env
      fun compileList [] _ list = list
        | compileList (arg::args) n list =
            compileList args (n-1) ((JSSetVar((nth(env', n), ~1), compileJSLambda arg env'))::list)
      val list' = compileList args (l-1) []
    in
      (list',env')
    end
  fun compileLetList args =
    let
      val l = (length args)
      val env' = updEnv l env
      fun compileList [] _ list = list
        | compileList (arg::args) n list =
            compileList args (n-1) ((JSSetVar((nth(env', n), ~1), compileJSLambda arg env))::list)
      val list' = compileList args (l-1) []
    in
      (list',env')
    end
in
  case exp of
    Llet(args, exp2) =>
      let val (list',env') = (compileLetList args) in extractLetList exp2 (list'@list) env' end
  | Lletrec(args, exp2) =>
      let val (list',env') = (compileLetrecList args) in extractLetList exp2 (list'@list) env' end
  | _ => (rev list,compileJSLambda exp env)
end

and compileCall (name, arity) args env =
  case (name, args) of
    ("sml_concat", arg1 :: arg2 :: []) => JSOperator (JSConcat, [compileJSLambda arg1 env, compileJSLambda arg2 env])
  | _ => let
      val arglist = map (fn arg => compileJSLambda arg env) args
    in
      if Char.contains name #"." then
        JSCall(name, arglist)
      else
        JSCall("$_mosmllib."^name, arglist)
    end

and compileBlock tag list env =
  JSBlock(tag, map (fn x => compileJSLambda x env) list)

and compileBlocksc tag sclist =
  JSBlock(tag, map (fn x => compileConst x) sclist)

and compileConst (ATOMsc(scon)) = compileSCon scon
  | compileConst (BLOCKsc(CONtag(tag,_), args)) = compileBlocksc tag args

and compileGetField (Lprim(Pget_global qualid, _)) idxs _ = (idxs, qualid)
  | compileGetField (Lvar(j)) idxs env = (idxs, (nth(env,j), ~1))
  | compileGetField (Lprim(Pfield(i),[arg])) idxs env = compileGetField arg (Int.toString(i)::idxs) env
;