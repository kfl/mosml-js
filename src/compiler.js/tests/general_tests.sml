(* struct *)
structure Struct_test :> Struct_test =
struct
  fun a x = x
  val b = 1
  val c = a b
end

(* type + datatype *)
type vector = real * real * real
val type_vector_test : vector = (1.5, 2.0, 3.1)

datatype kbool = true | false