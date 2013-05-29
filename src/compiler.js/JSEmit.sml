(* JSEmit.sml : Emition of intermediate JS language 
   from JSBack.sml and defined in JSInstruct.sml
   Author: Jens Fredskov, Henrik Bendt
   Date: 2013-05-29
*)

local
  open JSInstruct Const TextIO List;
  val outstream = ref stdOut;
  val arch = Option.valOf(Int.precision);
  val jslib = "$_mosmllib."
  val overflowCheck = if arch = 63 then jslib^"overflowCheck64(" else jslib^"overflowCheck32(";
  val wordJSToSml = if arch = 63 then jslib^"wordJSToSml64(" else jslib^"wordJSToSml32(";
  val wordSmlToJS = jslib^"wordSmlToJS("
  val Constructor = jslib^"Constructor("
  val divInt = jslib^"divInt("
  val division = jslib^"division("
in

(* Prints out string s to outstream. *)
  fun out (s : string) =
    output (!outstream, s);
  ;

(* Emits constants. *)
  fun outConst scon =
    case scon of
      JSINTscon i => out i
    | JSWORDscon w => out w
    | JSREALscon r => out r
    | JSSTRscon s => out ("\""^s^"\"")
  ;
(* Emits qualid, i.e. name, of variables and append index.
   Also handles special characters not usable in JS. *)
  fun outQualid ((qual : QualifiedIdent), idx) =
    let
      (* Handle invalid names in JS*)
      val handleInvalidNames = fn
        #"@" => "$_at"
      | #"!" => "$_bang"
      | #"'" => "$_mark"
      | n => Char.toString(n)
    in
      (out (String.translate handleInvalidNames (hd (#id qual)));
       if idx > 0 then (out "$"; out (Int.toString idx)) else ())
    end;

  (*Emit the given phrase in intermediate JS language defined in JSInstruct.sml.*)
  fun emit jsinstr =
    case jsinstr of
      JSOperator(op1, [js]) =>
        (case op1 of
          JSNegNum => (out "-"; emit js)
        | JSStringLength => (emit js; out ".length")
        | _ => out "/*ERROR: JSOperator*/"
        )
    | JSOperator(op1, [js1, js2]) =>
        (case op1 of
          JSConcat => (emit js1; out "+"; emit js2)
        | JSAddInt => (out overflowCheck; emit js1; out "+"; emit js2; out ")")
        | JSSubInt => (out overflowCheck; emit js1; out "-"; emit js2; out ")")
        | JSMulInt => (out overflowCheck; emit js1; out "*"; emit js2; out ")")
        | JSDivInt => (out overflowCheck; out divInt; emit js1; out ","; emit js2; out "))")
        | JSModInt => (out overflowCheck; emit js1; out "%"; emit js2; out ")")
        | JSAddFloat => (emit js1; out "+"; emit js2)
        | JSSubFloat => (emit js1; out "-"; emit js2)
        | JSMulFloat => (emit js1; out "*"; emit js2)
        | JSDivFloat => (out division; emit js1; out ","; emit js2; out ")")
        | JSAddWord => (out wordJSToSml; out wordSmlToJS; emit js1; out ") + "; out wordSmlToJS; emit js2; out "))")
        | JSSubWord => (out wordJSToSml; out wordSmlToJS; emit js1; out ") - "; out wordSmlToJS; emit js2; out "))")
        | JSMulWord => (out wordJSToSml; out wordSmlToJS; emit js1; out ") * "; out wordSmlToJS; emit js2; out "))")
        | JSDivWord => (out wordJSToSml; out wordSmlToJS; emit js1; out ") "; out divInt; out wordSmlToJS; emit js2; out ")))")
        | JSModWord => (out wordJSToSml; out wordSmlToJS; emit js1; out ") % "; out wordSmlToJS; emit js2; out "))")
        | _ => out "/*ERROR: JSOperator*/"
        )
    | JSConst c => outConst c
    | JSGetVar qualid => outQualid qualid
    | JSFun(JSScope(jss, js), qualid) =>
        (out "function("; outQualid qualid; out "){\n"; scopeLoop jss; 
         out "return "; emit js; out ";}")
    | JSFun(js, qualid) => (out "function("; outQualid qualid; out ")\n{"; 
                            out "return "; emit js; out ";}")
    | JSIf(tst, js1, js2) =>
      (case tst of
        JSTest(_,_,_) =>
          outAnon (fn _ => (out " return ("; emit tst; out ".tag ? "; emit js1; out " : "; emit js2; out ")"))
      | _ =>
          outAnon (fn _ => (out " return ("; emit tst; out ".tag ? "; emit js1; out " : "; emit js2; out ")"))
      )
    | JSSetField(i, js1, js2) =>
        outAnon (fn _ => (emit js1; out ".args["; out (Int.toString i); out "] = "; emit js2))
    | JSSetVar(qualid, js) => (out "var "; outQualid qualid; out " = "; emit js)
    | JSScope(jss, js) =>
      (case js of
        (JSSetVar(qualid, js)) =>
          (out "var "; outQualid qualid; out " = ";
	         outAnon (fn _ => (out "\n"; scopeLoop jss; out "return "; emit js; out ";\n")))
      | (JSSeqFun(js1, js2)) =>
        let
          fun evalVars (js1, js2) vars vals =
            case (js1,js2) of
              (JSSetVar(qualid1, js1), JSSetVar(qualid2, js2)) =>
                (rev(qualid1::qualid2::vars),
                 rev(js2::js1::vals))
            | (JSSetVar(qualid, js1), JSSeqFun(js2, js3))      =>
                evalVars (js2, js3) (qualid::vars) (js1::vals)

          val (vars, vals) = evalVars (js1,js2) [] []

          fun outVars (js::[])  = outQualid js
            | outVars (js::jss) = (outQualid js; out ","; outVars jss)

          fun outVals (js1::[]) (js2::[])  = (outQualid js1; out " = "; emit js2; out ";\n")
            | outVals (js1::jss1) (js2::jss2) = (outQualid js1; out " = "; emit js2; out ";\n"; outVals jss1 jss2)
            | outVals _ _ = out "Error in JSSeqFun\n"
        in
          (out "var "; outVars vars; out ";\n";
           outAnon (fn _ => (out "\n"; scopeLoop jss; outVals vars vals; out "return ;\n")))
(*    (out "var ["; out vars; out "] = "; outAnon (fn _ => (out "\n"; scopeLoop jss; out "return ["; outVals vals; out "];\n"))) *)
        end
      | _ => outAnon (fn _ => (out "\n"; scopeLoop jss; out "return "; emit js; out ";\n"))
      )
    | JSTest(tst, js1, js2) =>
      (case tst of
        JSeq          => (out Constructor; emit js1; out " == "; emit js2; out "? 1 : 0)")
      | JSneq         => (out Constructor; emit js1; out " != "; emit js2; out "? 1 : 0)")
      | JSlt          => (out Constructor; emit js1; out " < "; emit js2; out "? 1 : 0)")
      | JSle          => (out Constructor; emit js1; out " <= "; emit js2; out "? 1 : 0)")
      | JSgt          => (out Constructor; emit js1; out " > "; emit js2; out "? 1 : 0)")
      | JSge          => (out Constructor; emit js1; out " >= "; emit js2; out "? 1 : 0)")
      )
    | JSNot(js) => (out "!"; emit js)
    | JSApply(func, args) => (emit func; emitArgs args)
    | JSSeq(js1, js2) => (out "("; emit js1; out ", "; emit js2; out ")")
    | JSSeqFun(js1, js2) => (emit js1; out ";\n"; emit js2)
    | JSAnd(js1, js2) => (emit js1; out " && "; emit js2)
    | JSOr(js1, js2) => (emit js1; out " || "; emit js2)
    | JSWhile(exp, body) => (out "(function(){while ("; emit exp; out ".tag){\n"; emit body; out "\n}}())")
    | JSUnspec => out ""
    | JSSwitch(0, exp, clist, def) =>
        outAnon (fn _ => (out "switch("; emit exp; out "){";
        map (fn (lbl, exp') => (out "\ncase "; emit lbl; out ":\nreturn ";
        emit exp')) clist; out "\ndefault:\nreturn "; emit def; out "\n}"))
    | JSSwitch(1, exp, clist, def) =>
        outAnon (fn _ => (out "{switch("; emit exp; out ".tag){";
        map (fn (lbl, exp') => (out "\ncase "; emit lbl; out ":\nreturn ";
        emit exp')) clist; out "\ndefault:\nreturn "; emit def; out "\n}}"))
    | JSBlock(tag, args) => outBlock tag args
    | JSGetField(idxs,qualid) =>
        (outQualid qualid; app (fn idx => (out ".args["; out idx; out "]")) idxs)
    | JSRaise(js) => outAnon (fn _ => (out "throw "; emit js))
    | JSTryCatch(js1, var, exp1, exp2, js, exp3) =>
        outAnon (fn _ => (out "try {\nreturn "; emit js1; out "\n} catch (";
                          emit var; out "){\n" ; out "if("; emit exp1; out " == ";
                          emit exp2; out "){\n return "; emit js; 
                          out ";\n}else{\nreturn "; emit exp3; out "\n}"; out "}"))
    | JSCall(call, args) => (out call; out "("; emitCallArgs args; out ")")
    | JSError(errmsg) => (out "/*ERROR: "; out errmsg; out "*/")
    | _ => out "/*ERROR: JSEmit*/"
    (* Emits arguments of JSCall (of Pccall). *)
    and emitCallArgs [] = ()
      | emitCallArgs (arg::[]) = emit arg
      | emitCallArgs (arg::args) = (emit arg; out ","; emitCallArgs args)
    (* Emits (curried) arguments to a funciton call. *)
    and emitArgs [] = ()
      | emitArgs (arg::args) = (out "("; emit arg; out ")"; emitArgs args)
    (* Emits the scope variables before the return statement. *)
    and scopeLoop [] = ()
      | scopeLoop (exp::exps) = (emit exp; out ";\n"; scopeLoop exps)
    (* Emits the block construcktor. *)
    and outBlock tag [] = (out Constructor; out (Int.toString tag); out ")")
      | outBlock tag (arg::args) =
        (out Constructor; out (Int.toString tag); out ",["; emit arg;
         map (fn x => (out ", "; emit x)) args; out "])")
    (* Emits an anonymous function around the given function. *)
    and outAnon f = (out "(function(){"; f (); out "}())")
  ;
  (* Emit the given JSInstruction (phrase) to the given outstream. *)
  fun emitPhrase os (ajs : JSInstruction) =
  (
    outstream := os;
    emit ajs;
    out ";\n";
    flushOut os
  );
end;
