local
  open Mixture Const Prim Lambda Asynt;
in

local fun print_js_id [] = ()
        | print_js_id [i] = msgString i
        | print_js_id (i::id) =
            (print_js_id id;  msgString "__" ; msgString i)
in
fun printJSQualId {qual="", id=id} =
      print_js_id id
  | printJSQualId {qual=qual, id=id} =
      (msgString qual;  msgString "__" ; print_js_id id)
end;

fun printJSConst scon =
    case scon of
      STRINGscon s => msgString ("\""^s^"\"")
    | _ => msgString "printJSConst" (* Debugger *)

fun printJSLam lam =
  case lam of
    Lconst (ATOMsc scon) => printJSConst scon
  | Lprim (prim, args) => printJSPrim prim args
  | _ => msgString "printJSLam" (* Debugger *)

  and printJSPrim prim args =
    case (prim, args) of
      (Pccall (name, arity), args) => printJSCall name args
    | (Pget_global (qualid, i), [])
        => (printJSQualId qualid; msgInt i)
    | (Pset_global (qualid, i), [lam])
        => (msgString "var "; printJSQualId qualid; msgInt i; msgString "="; printJSLam lam)
    | (_, _) => msgString "printJSPrim" (* Debugger *)

  and printJSCall name args =
    case (name, args) of
      ("sml_concat", a1::a2::[])
        => (msgString "("; printJSLam a1; msgString "+";
            printJSLam a2; msgString ")")
    |  (_, _) => msgString "printJSCall" (* Debugger *)
;
end;