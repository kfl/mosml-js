val it = 1;
val it = 2 + ((fn 2 => 99 | x => x) 3);

val it = let val x=99 in x+1 end;

val rec fact = fn 0 => 1 | n => n * fact(n-1);
val it = fact 4;

val rec append2 =
  fn ([], ys) => ys
   | (x::xs, ys) => x :: append2 (xs,ys)
;
val it = append2( [1,2,3], [4,5,6] );

val rec append =
  fn xs => fn ys =>
    case xs
      of [] => ys
       | x :: xs => x :: append xs ys
;
val it = append [1,2,3] [4,5,6];

val reverse = fn xs =>
  let
    val rec loop = fn ([],ys) => ys
                    | (x::xs,ys) => loop (xs,x::ys)
  in
    loop (xs, []) end;
val it = reverse [1,2,3,4];
val it = reverse [true,false];

val op @ = append2;
infixr 5 @;
val it = [1,2,3] @ [4,5,6];
