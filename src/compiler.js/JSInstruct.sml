(* The type of the instructions of the abstract machine *)

(* 1996.07.05 -- e *)

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
;
