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
  (*  Kquote of StructConstant
  | Kget_global of QualifiedIdent * int
  | Kset_global of QualifiedIdent * int
  | Kaccess of int
  | Kenvacc of int                  (* new *)
  | Kassign of int                  (* newer *)
  | Kgetfield of int                (* new *)
  | Ksetfield of int                (* new *)
  | Kpush
  | Kpop of int                     (* added arg *)
  | Krestart                        (* new *)
  | Kgrab of int                    (* added arg *)
  | Kapply of int                   (* added arg *)
  | Kappterm of int * int           (* added args and renamed *)
  | Kpush_retaddr of int            (* new *)
  | Kcheck_signals
  | Kreturn of int                  (* added arg *)
  | Kclosure of int * int           (* added arg *)
  | Kclosurerec of int * int        (* new *)
  | Kraise                          (* new *)
  | Kmakeblock of BlockTag * int
  | Kprim of primitive
  | Kpushtrap of int
  | Kpoptrap
  | Klabel of int
  | Kbranch of int
  | Kbranchif of int
  | Kbranchifnot of int
  | Kstrictbranchif of int
  | Kstrictbranchifnot of int
  | Ktest of bool_test * int
  | Kbranchinterval of int * int * int * int
  | Kswitch of int Array.array *)

type JSPhrase =
{
  js_funcs:   JSInstruction list,     (* code for functions *)
  js_inits:   JSInstruction list,     (* initialization code *)
  js_is_pure: bool                     (* pure = no side effects *)
};
