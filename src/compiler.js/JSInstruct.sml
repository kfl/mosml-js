(* The description of the intermediate JS-instruction set. *)

open Const Prim;

datatype JSSCon =
    JSINTscon of string
  | JSWORDscon of string
  | JSREALscon of string
  | JSSTRscon of string

datatype JSBool =
    JSTrue | JSFalse

datatype JSInstruction =
    JSGetVar of QualifiedIdent
  | JSGetList of string * QualifiedIdent
  | JSSetVar of QualifiedIdent * JSInstruction
  | JSConst of JSConstant
  | JSAdd of JSOp * JSInstruction * JSInstruction
  | JSSub of JSOp * JSInstruction * JSInstruction
  | JSMul of JSOp * JSInstruction * JSInstruction
  | JSDiv of JSOp * JSInstruction * JSInstruction
  | JSMod of JSOp * JSInstruction * JSInstruction
  | JSFun of JSInstruction * QualifiedIdent
  | JSScope of JSInstruction list * JSInstruction
  | JSIf of JSInstruction * JSInstruction * JSInstruction
  | JSNot of JSInstruction
  | JSTest of JSPrimTest * JSInstruction * JSInstruction
  | JSApply of JSInstruction * JSInstruction list
  | JSError of int (* Note: this is just for debugging purposes. *)

and JSConstant =
    JSATOMsc of JSSCon
  | JSLISTsc of JSInstruction list
  | JSBoolsc of JSBool

and JSOp =
    JSAddInt | JSSubInt | JSMulInt | JSDivInt | JSModInt | JSConcat

and JSPrimTest =
    JSeq | JSnoteq | JSlt | JSle | JSgt | JSge
;
