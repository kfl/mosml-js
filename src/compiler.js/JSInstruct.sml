(* The description of the intermediate JS-instruction set. *)

open Const Prim;

datatype JSSCon =
    JSINTscon of string
  | JSWORDscon of string
  | JSREALscon of string
  | JSSTRscon of string

datatype JSConstant =
    JSATOMsc of JSSCon
  | JSLISTsc of JSConstant list
;

datatype JSInstruction =
    JSGetVar of QualifiedIdent
  | JSSetVar of QualifiedIdent * JSInstruction
  | JSConst of JSConstant
  | JSAdd of JSOp * JSInstruction * JSInstruction
  | JSSub of JSOp * JSInstruction * JSInstruction
  | JSMul of JSOp * JSInstruction * JSInstruction
  | JSDiv of JSOp * JSInstruction * JSInstruction
  | JSMod of JSOp * JSInstruction * JSInstruction
  | JSFun of JSInstruction * QualifiedIdent
  | JSScope of JSInstruction list * JSInstruction
  | JSError of int (* Note: this is just for debugging purposes. *)

and JSOp =
  JSAddInt | JSSubInt | JSMulInt | JSDivInt | JSModInt | JSConcat
;
