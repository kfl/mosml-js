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
    | JSFun (list,js) => (out ("(function()\n{"); scopeLoop list 0; out "return "; emit js; out ";\n}())") (*anonymous function*)
    | JSSetVar(qualid, js) => (out ("var "^(hd(#id qualid))^" = "); emit js)
    | JSSetVar(qualid, (JSFun(list, js))) => (out ("var "^(hd(#id qualid))^" = function ()\n{"); scopeLoop list 0; emit js; out "\n}") (*function*)
    | JSVar(i) => out ("var"^Int.toString(i))
    | _ => out " Error! "

    and emitList jsinstrlist = ()

    and scopeLoop [] _ = ()
      | scopeLoop (exp::exps) i = (out ("var var"^Int.toString(i)^" = "); emit exp; out ";\n"; scopeLoop exps (i+1))
  ;

  fun emitPhrase os (ajs : JSInstruction) =
  (
    outstream := os;
    emit ajs;
    out ";\n";
    flushOut os
  );
end;