fun fact 0 = 1
  | fact n = n * fact(n-1);

val it = fact 4;

fun append2 ([], ys) = ys
  | append2 (x::xs, ys) = x :: append2 (xs,ys);

val it = append2( [1,2,3], [4,5,6] );

fun append [] ys = ys
  | append (x :: xs) ys = x :: append xs ys;

val it = append [1,2,3] [4,5,6];

fun reverse xs =
  let fun loop [] ys = ys
        | loop (x::xs) ys = loop xs (x::ys)
  in loop xs [] end;

val it = reverse [1,2,3,4];
val it = reverse [true,false];

val op @ = append2;
infixr 5 @;
val it = [1,2,3] @ [4,5,6];
