prim_val clog_ : 'a -> unit = 1 "console.log";

fun clog x = clog_ x;

fun clogBool true = clog "true"
  | clogBool false = clog "false";

(* Lshared *)
datatype Lshared_datatype = A | B
fun Lshared_test x =
    case x of
        (A,A,_,_,_,_,_,_,_,_) => 0
      | (_,_,A,A,_,_,_,_,_,_) => 1
      | (_,_,_,_,A,A,_,_,_,_) => 2
      | (_,_,_,_,_,_,A,A,_,_) => 3
      | (_,_,_,_,_,_,_,_,A,A) => 4
      | (A,B,A,B,A,B,A,B,A,B) => ~1

val _  = clogBool ((Lshared_test (A,A,B,B,B,B,B,B,B,B)) = 0)
val _  = clogBool ((Lshared_test (B,B,A,A,B,B,B,B,B,B)) = 1)
val _  = clogBool ((Lshared_test (B,B,B,B,A,A,B,B,B,B)) = 2)
val _  = clogBool ((Lshared_test (B,B,B,B,B,B,A,A,B,B)) = 3)
val _  = clogBool ((Lshared_test (B,B,B,B,B,B,B,B,A,A)) = 4)
val _  = clogBool ((Lshared_test (A,B,A,B,A,B,A,B,A,B)) = ~1)