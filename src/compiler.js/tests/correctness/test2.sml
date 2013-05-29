prim_val clog_ : 'a -> unit = 1 "console.log"

fun clog x = clog_ x

fun clogBool true = clog "true"
  | clogBool false = clog "false"

fun fact 0 = 1
  | fact n = n * fact(n-1);

val _ = clogBool(fact 4 = 24);

fun append2 ([], ys) = ys
  | append2 (x::xs, ys) = x :: append2 (xs,ys);

val _ = clogBool (append2([1,2,3], [4,5,6]) = [1,2,3,4,5,6]);

fun append [] ys = ys
  | append (x :: xs) ys = x :: append xs ys;

val _ = clogBool (append [1,2,3] [4,5,6] = [1,2,3,4,5,6]);

fun reverse xs =
  let fun loop [] ys = ys
        | loop (x::xs) ys = loop xs (x::ys)
  in loop xs [] end;

val _ = clogBool (reverse [1,2,3,4] = [4,3,2,1]);
val _ = clogBool (reverse [true,false] = [false, true]);

val op @ = append2;
infixr 5 @;
val _ = clogBool ([1,2,3] @ [4,5,6] = [1,2,3,4,5,6]);
