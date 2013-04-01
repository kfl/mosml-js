open JSInstruct Const TextIO;
local
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
      JSAdd(a,b) => (emit a; out "+"; emit b)
    | JSConst(JSATOMsc k) => outConst k
    | JSConst(JSLISTsc l) => (out "[";outList l;out "]")
    | JSGetVar qualid => out (#qual qualid)
    | JSSetVar(qualid, js) => (out ((#qual qualid)^"="); emit js; out "; ")
    | _ => out " Error! "

    and emitList jsinstrlist = ()
  ;

  fun emitPhrase os (ajs : JSInstruction) =
  (
    outstream := os;
    emit ajs;
    flushOut os
  );
end;