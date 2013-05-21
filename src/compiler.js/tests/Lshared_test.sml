(* Lshared *)
datatype Lshared_datatype = Lshared_A | Lshared_B
fun Lshared_test x =
    case x of
        (Lshared_A,Lshared_A,_,_,_,_,_,_,_,_) => 0
      | (_,_,Lshared_A,Lshared_A,_,_,_,_,_,_) => 1
      | (_,_,_,_,Lshared_A,Lshared_A,_,_,_,_) => 2
      | (_,_,_,_,_,_,Lshared_A,Lshared_A,_,_) => 3
      | (_,_,_,_,_,_,_,_,Lshared_A,Lshared_A) => 4
      | (Lshared_A,Lshared_B,Lshared_A,Lshared_B,Lshared_A,Lshared_B,Lshared_A,Lshared_B,Lshared_A,Lshared_B) => ~1