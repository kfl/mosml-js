local
  open JSInstruct Const TextIO List;
  val outstream = ref stdOut;
  val arch = Option.valOf(Int.precision);
in

  fun out (s : string) =
    output (!outstream, s);
  ;

  fun outConst scon =
    case scon of
      JSINTscon i => out i
    | JSWORDscon w => out w
    | JSREALscon r => out r
    | JSSTRscon s => out ("\""^s^"\"")
  ;

  val overflowCheck = if arch = 63 then "overflowCheck64(" else "overflowCheck32(";

  (*Emit the given phrase in abstract js language defined in JSInstruct.sml.*)
  fun emit jsinstr =
    case jsinstr of
      JSOperator(op1, js1, js2) => 
        (case op1 of 
          JSConcat => (emit js1; out "+"; emit js2)
        | JSAddInt => (out overflowCheck; emit js1; out "+"; emit js2; out ")")
        | JSSubInt => (out overflowCheck; emit js1; out "-"; emit js2; out ")")
        | JSMulInt => (out overflowCheck; emit js1; out "*"; emit js2; out ")")
        | JSDivInt => (out overflowCheck; out "division("; emit js1; out ","; emit js2; out "))")
        | JSModInt => (out overflowCheck; emit js1; out "%"; emit js2; out ")")
      (*  | JSNegFloat =>  *)
        | JSAddFloat => (emit js1; out "+"; emit js2)
        | JSSubFloat => (emit js1; out "-"; emit js2)
        | JSMulFloat => (emit js1; out "*"; emit js2)
        | JSDivFloat => (out "division("; emit js1; out ","; emit js2; out ")")
        | _ => out "/*ERROR: JSOperator*/"
        )
    | JSConst c => outConst c
    | JSGetVar qualid => out (hd(#id qualid))
    | JSFun(JSScope(jss, js), qualid) =>
        (out ("function("^(hd(#id qualid))^"){\n"); scopeLoop jss; out "return "; emit js; out ";}")
    | JSFun(js, qualid) => (out ("function("^(hd(#id qualid))^")\n{"); out "return "; emit js; out ";}")
    | JSIf(tst, js1, js2) =>
      (case tst of
        JSTest(_,_,_) =>
          (out "(function(){ return ("; emit tst; out " ? "; emit js1; out " : "; emit js2; out ")}())")
      | _ =>
          (out "(function(){ return ("; emit tst; out " ? "; emit js1; out " : "; emit js2; out ")}())")
      )
    | JSSetVar(qualid, js) => (out ("var "^(hd(#id qualid))^" = "); emit js)
    | JSScope(jss, js) =>
      (case js of
        (JSSetVar(qualid, js)) =>
          (out ("var "^(hd(#id qualid))^" = "); out "(function(){\n";
           scopeLoop jss; out "return "; emit js; out ";\n}())")
      | _                      => (out "(function(){\n"; scopeLoop jss; out "return "; emit js; out ";\n}())")
      )
    | JSTest(tst, js1, js2) =>
      (case tst of
        JSeq          => (out "("; emit js1; out " === "; emit js2; out ")")
      | JSneq         => (out "("; emit js1; out " !== "; emit js2; out ")")
      | JSlt          => (out "("; emit js1; out " < "; emit js2; out ")")
      | JSle          => (out "("; emit js1; out " <= "; emit js2; out ")")
      | JSgt          => (out "("; emit js1; out " > "; emit js2; out ")")
      | JSge          => (out "("; emit js1; out " >= "; emit js2; out ")")
      )
    | JSNot(js) => (out "!"; emit js)
    | JSApply(func, args) => (emit func; emitArgs args)
    | JSSeq(js1, js2) => (out "("; emit js1; out ", "; emit js2; out ")")
    | JSSeqFun(js1, js2) => (emit js1; out ";\n"; emit js2)
    | JSAnd(js1, js2) => (emit js1; out " && "; emit js2)
    | JSOr(js1, js2) => (emit js1; out " || "; emit js2)
    | JSWhile(exp, body) => (out "while ("; emit exp; out "){\n"; emit body; out "\n}")
    | JSUnspec => out ""
    | JSSwitch(0, exp, clist, def) =>
        (out "(function(){switch("; emit exp; out "){";
         map (fn (lbl, exp') => (out "\ncase "; emit lbl; out ":\nreturn ";
         emit exp')) clist; out "\ndefault:\nreturn "; emit def; out "\n}}())")
    | JSSwitch(1, exp, clist, def) =>
        (out "(function(){switch("; emit exp; out ".tag){";
         map (fn (lbl, exp') => (out "\ncase "; emit lbl; out ":\nreturn ";
         emit exp')) clist; out "\ndefault:\nreturn "; emit def; out "\n}}())")
    | JSBlock(tag, args) => outBlock tag args
    | JSGetField(idxs,qualid) =>
        (out (hd(#id qualid)); app (fn idx => out (".args["^idx^"]")) idxs)
    | JSRaise(js) => (out "(function(){throw "; emit js; out "}())")
    | JSTryCatch(js1, var, exp1, exp2, js3) =>
        (out "(function(){try {\nreturn "; emit js1; out "\n} catch ("; emit var; out " if "; emit exp1; out " === ";
          emit exp2; out "){\n return "; emit js3; out "\n}}())")
    | JSError(errmsg) => (out "/*ERROR: "; out errmsg; out "*/")
    | _ => out "/*ERROR: JSEmit*/"

    and emitArgs [] = ()
      | emitArgs (arg::args) = (out "("; emit arg; out ")"; emitArgs args)

    and scopeLoop [] = ()
      | scopeLoop (exp::exps) = (emit exp; out ";\n"; scopeLoop exps)

    and outBlock tag [] = out ("Constructor("^(Int.toString tag)^")")
      | outBlock tag (arg::args) =
        (out ("Constructor("^(Int.toString tag)^",["); emit arg;
         map (fn x => (out ", "; emit x)) args; out "])")
  ;

  fun emitPhrase os (ajs : JSInstruction) =
  (
    outstream := os;
    emit ajs;
    out ";\n";
    flushOut os
  );
end;