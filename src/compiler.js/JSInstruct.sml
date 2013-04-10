(* The description of the intermediate JS-instruction set. *)

open Const Prim;

datatype JSSCon =
    JSNUMscon of string
  | JSSTRscon of string

datatype JSConstant =
    JSATOMsc of JSSCon
  | JSLISTsc of JSConstant list
;

datatype JSInstruction =
    JSGetVar of QualifiedIdent
  | JSSetVar of QualifiedIdent * JSInstruction
  | JSConst of JSConstant
  | JSAdd of JSInstruction * JSInstruction
  | JSFun of JSInstruction list * JSInstruction
  | JSVar of int
  | JSError of int (* Note: this is just for debugging purposes. *)
;
