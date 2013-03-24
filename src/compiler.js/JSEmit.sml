open JSInstruct Const Buffcode;

fun outConst (JSNUMscon i) = out i
  | outConst (JSSTRscon s) = out "\""^s^"\"";

fun outList [] = ()
  | outList s :: [] = outConst s
  | outList s :: ss = outConst s; out ","; outList ss


(*Emit the given phrase in abstract js language defined in JSInstruct.sml.*)
fun emit jsphrases =
    case jsphrases of
      [] => ()
    | JSAdd(a,b) :: c => (emit [a]; out "+"; emit [b])
    | JSConst(JSATOMsc k) :: c => (outConst k; emit c)
    | JSConst(JSLISTsc l) :: c => (out "[";outList l;out "]";  emit c)
    | JSGetVar(_,qual):: c => (out qual; emit c)
    | JSSetVar((_,qual), js) :: c => (out qual^"="; emit [js]; out "; "; emit c)
    | _ :: c => (out " Error! "; emit c)
;