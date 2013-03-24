(* The type of the instructions of the abstract machine *)

(* 1996.07.05 -- e *)

open Config Const Prim;

datatype JSConstant =
    ATOMsc of SCon
  | LISTsc of SCon List
;

datatype JSInstruction =
    JSGetVar of QualifiedIdent * int
  | JSSetVar of QualifiedIdent * int * JSInstruction
  | JSConst of JSConstant
  | JSAdd of JSInstruction * JSInstruction
;

type JSPhrase = JSInstruction list;
