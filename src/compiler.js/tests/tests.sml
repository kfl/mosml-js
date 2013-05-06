(* Lvar *)
val hello = "goodbye"
val Lvar_concat =
  let
    val hello = "hello"
    val world = "world"
    val hw = hello^world
  in
    hw
  end

(* Lconst *)
val Lconst_ATOMsc_INTscon = 15
val Lconst_ATOMsc_WORDscon = 0wx0F
val Lconst_ATOMsc_CHARscon = #"F"
val Lconst_ATOMsc_REALscon = 15.0
val Lconst_ATOMsc_STRINGscon = "fifteen"

val Lconst_BLOCKsc_list = [1, 2, 3, 4, 5]
val Lconst_BLOCKsc_tuple = (1, 2, 3, 4, 5)
val Lconst_BLOCKsc_false = false
val Lconst_BLOCKsc_true = true
val Lconst_BLOCKsc_ref = ref 10
datatype 'a Lconst_BLOCKsc_datatype =
  Lconst_BLOCKsc_datatype_nil
| 'a * 'a Lconst_BLOCKsc_datatype

(* Lfn + Lapply *)
fun Lfn_normal (x,y) = x+1+y;
val Lapply_normal = Lfn_normal (1,2)

fun Lfn_curried x y = x + y;
val Lapply_closure1 = Lfn_curried 2
val Lapply_closure2 = Lapply_closure1 3
val Lapply_closure3 = Lfn_curried 2 3

(* Llet *)
val a = 2
val Llet_test =
  let
    val a = 1
    val b = 1
  in
    a+b = 2
  end

(* Lletrec *)
fun Lletrec_test1 x = x+x
val Lletrec_test2 y =
  let
    fun Lletrec_test1 x = x
  in
    Lletrec_test1 y
  end
val Lletrec_test3 = Lletrec_test2 2 = 2

(* Lprim *)

(* Lstaticfail *)

(* Lhandle *)
val Lhandle_test = 10/0 handle Div => 0

(* Lstatichandle -> Lcase *)
fun Lcase_fib_rec 0 = 1
  | Lcase_fib_rec 1 = 1
  | Lcase_fib_rec n = Lcase_fib_rec n-1 + Lcase_fib_rec n-2

fun Lcase_fib_case n =
  case n of
    0 => 1
  | 1 => 1
  | n => Lcase_fib_case n-1 + Lcase_fib_case n-2

fun Lcase_not_exhaustive 1 = 1
  | Lcase_not_exhaustive 2 = 2

fun Lcase_curried 1 1 = 2
  | Lcase_curried n 1 = Lcase_curried (n-1) 1 + Lcase_curried 1 1
  | Lcase_curried 1 n = Lcase_curried (n-1) 1

fun Lcase_list [] = 0
  | Lcase_list (x::xs) = 1 + Lcase_list xs

(* Lstatichandle -> Lswitch *)

(* Lif *)
val Lif_a = 63;
val Lif_b = if Lif_a = 63 then 10 else 20;
val Lif_c = if Lif_b = 20 then "a" else "b";

val Lif_d = false;
val Lif_d = true;
val Lif_e = if Lif_d then 1 else 2;
val Lif_f = if not Lif_d then 1 else 2;

fun Lif_test x : bool = if x then 1 else 0;

(* Lseq *)
val Lseq_discard_evaluation = (1; 2)

fun Lseq_func1 0 = 0
  | Lseq_func1 x = 1 + Lseq_func2 x-1
and Lseq_func2 0 = 0
  | Lseq_func2 x = 2 * Lseq_func1 x-1

(* Lwhile *)
val Lwhile_infinite = while true do 1;

(* Landalso + Lorelse *)
val Landalso_test1 = (true andalso false) = false
val Lorelse_test = (false orelse true) = true

(* Lunspec *)
val _ = "Lunspec"