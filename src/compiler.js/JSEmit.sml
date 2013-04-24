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

  fun outList [] = ()
    | outList ((JSLISTsc s) :: []) = (out "["; outList s; out "]")
    | outList ((JSATOMsc s) :: [])= outConst s
    | outList ((JSLISTsc s) :: ss) = (out "["; outList s;out "]"; out ","; outList ss)
    | outList ((JSATOMsc s) :: ss) = (outConst s; out ","; outList ss)
  ;

  val overflowCheck = if arch = 63 then "overflowCheck64(" else "overflowCheck32(";

  (*Emit the given phrase in abstract js language defined in JSInstruct.sml.*)
  fun emit jsinstr =
    case jsinstr of
      JSAdd(JSConcat, js1, js2) => (emit js1; out "+"; emit js2)
    | JSAdd(JSAddInt, js1, js2) => (out overflowCheck; emit js1; out "+"; emit js2; out ")")
    | JSSub(JSSubInt, js1, js2) => (out overflowCheck; emit js1; out "-"; emit js2; out ")")
    | JSMul(JSMulInt, js1, js2) => (out overflowCheck; emit js1; out "*"; emit js2; out ")")
    | JSDiv(JSDivInt, js1, js2) => (out overflowCheck; out "division("; emit js1; out ","; emit js2; out "))")
    | JSMod(JSModInt, js1, js2) => (out overflowCheck; emit js1; out "%"; emit js2; out ")")
    | JSConst(JSATOMsc k) => outConst k
    | JSConst(JSLISTsc l) => (out "["; outList l; out "]")
    | JSGetVar qualid => out (hd(#id qualid))
    | JSGetList (i,qualid) => (out (hd(#id qualid)^"["^i^"]"))
    | JSFun(JSScope(jss, js), qualid) => 
        (out ("function("^(hd(#id qualid))^"){\n"); scopeLoop jss; out "return "; emit js; out ";}")
    | JSFun(js, qualid) => (out ("function("^(hd(#id qualid))^")\n{"); out "return "; emit js; out ";}")
    | JSIf(tst, js1, js2) => 
      (case tst of 
        JSTest(_,_,_) => 
          (out "(function(){ return ("; emit tst; out "? "; emit js1; out " : "; emit js2; out ")}())")
      | _ => 
          (out "(function(){ return ("; emit tst; out "? "; emit js1; out " : "; emit js2; out ")}())")
      )
    | JSSetVar(qualid, js) => (out ("var "^(hd(#id qualid))^" = "); emit js)
    | JSScope(jss, js) => (out "(function(){\n"; scopeLoop jss; out "return "; emit js; out ";\n}())")
    | JSTest(tst, js1, js2) => 
      (case tst of
        JSeq => (out "("; emit js1; out " === "; emit js2; out ")")
      | _ => out " Error! "
      )
    | JSNot(js) => (out "!"; emit js)
    | JSApply(func, args) => (emit func; emitArgs args)
    | _ => out " Error! "

    and emitArgs [] = ()
      | emitArgs (arg::args) = (out "("; emit arg; out ")"; emitArgs args)

    and scopeLoop [] = ()
      | scopeLoop (exp::exps) = (emit exp; out ";\n"; scopeLoop exps)
  ;

  fun emitPhrase os (ajs : JSInstruction) =
  (
    outstream := os;
    emit ajs;
    out ";\n";
    flushOut os
  );
end;