local
  open JSInstruct Const TextIO List;
  val outstream = ref stdOut;
in

  fun out (s : string) =
    output (!outstream, s);
  ;

  fun outConst (JSNUMscon i) = out i
    | outConst (JSSTRscon s) = out ("\""^s^"\"")
  ;
  fun outList [] = ()
    | outList ((JSLISTsc s) :: []) = (out "["; outList s; out "]")
    | outList ((JSATOMsc s) :: [])= outConst s
    | outList ((JSLISTsc s) :: ss) = (out "["; outList s;out "]"; out ","; outList ss)
    | outList ((JSATOMsc s) :: ss) = (outConst s; out ","; outList ss)
  ;

  (*Emit the given phrase in abstract js language defined in JSInstruct.sml.*)
  fun emit jsinstr =
    case jsinstr of
      JSAdd(js1,js2) => (emit js1; out "+"; emit js2)
    | JSConst(JSATOMsc k) => outConst k
    | JSConst(JSLISTsc l) => (out "["; outList l; out "]")
    | JSGetVar qualid => out (hd(#id qualid))
    | JSFun(JSScope(jss, js), qualid) => 
        (out ("function("^(hd(#id qualid))^"){\n"); scopeLoop jss; out "return "; emit js; out ";\n}")
    | JSFun(js, qualid) => (out ("function("^(hd(#id qualid))^")\n{"); out "return "; emit js; out ";\n}")
    | JSSetVar(qualid, js) => (out ("var "^(hd(#id qualid))^" = "); emit js)
    | JSScope(jss, js) => (out "(function(){\n"; scopeLoop jss; out "return "; emit js; out ";\n}())")
    | _ => out " Error! "

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