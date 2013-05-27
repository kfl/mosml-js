(* The description of the intermediate JS-instruction set. *)

open Const Prim;

datatype JSConstant =
    JSINTscon of string
  | JSWORDscon of string
  | JSREALscon of string
  | JSSTRscon of string

datatype JSInstruction =
    JSGetVar of QualifiedIdent
  | JSGetField of string list * QualifiedIdent
  | JSSetField of int * JSInstruction * JSInstruction
  | JSSetVar of QualifiedIdent * JSInstruction
  | JSConst of JSConstant
  | JSOperator of JSOp * JSInstruction list
  | JSFun of JSInstruction * QualifiedIdent
  | JSScope of JSInstruction list * JSInstruction
  | JSIf of JSInstruction * JSInstruction * JSInstruction
  | JSNot of JSInstruction
  | JSTest of JSTestType * JSInstruction * JSInstruction
  | JSApply of JSInstruction * JSInstruction list
  | JSSeq of JSInstruction * JSInstruction
  | JSSeqFun of JSInstruction * JSInstruction
  | JSAnd of JSInstruction * JSInstruction
  | JSOr of JSInstruction * JSInstruction
  | JSWhile of JSInstruction * JSInstruction
  | JSUnspec
  | JSSwitch of int * JSInstruction * (JSInstruction * JSInstruction) list * JSInstruction
  | JSBlock of int * JSInstruction list
  | JSRaise of JSInstruction (* TODO might redesign this *)
(*  | JSTryCatch of JSInstruction * JSInstruction * JSInstruction * JSInstruction * JSInstruction *)
  | JSTryCatch of JSInstruction * JSInstruction * (JSInstruction * JSInstruction * JSInstruction) list
  | JSCall of string * JSInstruction list
  | JSError of string (* Note: this is just for debugging purposes. *)

and JSOp =
    JSAddInt | JSSubInt | JSMulInt | JSDivInt | JSModInt | JSConcat
  | JSAddFloat | JSSubFloat | JSMulFloat | JSDivFloat
  | JSAddWord | JSSubWord | JSMulWord | JSDivWord | JSModWord | JSAndWord
  | JSNegNum | JSStringLength

and JSTestType =
    JSeq | JSneq | JSlt | JSle | JSgt | JSge
;