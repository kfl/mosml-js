prim_val clog_ : 'a -> unit = 1 "console.log"

fun clog x = clog_ x

fun clogBool true = clog "true"

fun clogBoolList ([] : int list) ([] : int list) = clog "true"
  | clogBoolList (x::xs) (y::ys) = if x = y then clogBoolList xs ys else clog "false"
  | clogBoolList _ _ = clog "false"

fun fact 0 = 1
  | fact n = n * fact(n-1);

val _ = clogBool(fact 4 = 24);

fun append2 ([], ys) = ys
  | append2 (x::xs, ys) = x :: append2 (xs,ys);

val it = clogBoolList (append2([1,2,3], [4,5,6])) [1,2,3,4,5,6];

fun append [] ys = ys
  | append (x :: xs) ys = x :: append xs ys;

val it = clogBoolList (append [1,2,3] [4,5,6]) [1,2,3,4,5,6];

fun reverse xs =
  let fun loop [] ys = ys
        | loop (x::xs) ys = loop xs (x::ys)
  in loop xs [] end;

val it = clogBoolList (reverse [1,2,3,4]) [4,3,2,1];
val it = reverse [true,false];

val op @ = append2;
infixr 5 @;
val it = clogBoolList ([1,2,3] @ [4,5,6]) [1,2,3,4,5,6];
