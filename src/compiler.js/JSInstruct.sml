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
  | JSTryCatch of JSInstruction * JSInstruction * JSInstruction * JSInstruction * JSInstruction
  | JSError of string (* Note: this is just for debugging purposes. *)

and JSOp =
    JSAddInt | JSSubInt | JSMulInt | JSDivInt | JSModInt | JSConcat

and JSTestType =
    JSeq | JSneq | JSlt | JSle | JSgt | JSge
    | JSneqtag of int (* maybe to be removed *)
;
(*
and bool_test =
    Peq_test
  | Pnoteq_test
  | Pint_test of int prim_test
  | Pfloat_test of real prim_test
  | Pstring_test of string prim_test
  | Pword_test of word prim_test
  | Pnoteqtag_test of BlockTag

and 'a prim_test =
    PTeq
  | PTnoteq
  | PTnoteqimm of 'a
  | PTlt
  | PTle
  | PTgt
  | PTge
*)
