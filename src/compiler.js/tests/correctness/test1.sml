prim_val clog_ : 'a -> unit = 1 "console.log";

fun clog x = clog_ x;

fun clogBoolList ([] : int list) ([] : int list) = clog "true"
  | clogBoolList (x::xs) (y::ys) = if x = y then clogBoolList xs ys else clog "false"
  | clogBoolList _ _ = clog "false"

fun clogBool true = clog "true"
  | clogBool false = clog "false";

val _ = clogBool(1 = 1);
val _ = clogBool(2 + ((fn 2 => 99 | x => x) 3) = 5);

val _ = let val x=99 in clogBool(x+1 = 100) end;

val rec fact = fn 0 => 1 | n => n * fact(n-1);
val _ = clogBool(fact 4 = 24);

val rec append2 =
  fn ([], ys) => ys
   | (x::xs, ys) => x :: append2 (xs,ys)
;
val _ = clogBoolList (append2([1,2,3], [4,5,6])) [1,2,3,4,5,6];

val rec append =
  fn xs => fn ys =>
    case xs
      of [] => ys
       | x :: xs => x :: append xs ys
;
val _ = clogBoolList (append [1,2,3] [4,5,6]) [1,2,3,4,5,6];

val reverse = fn xs =>
  let
    val rec loop = fn ([],ys) => ys
                    | (x::xs,ys) => loop (xs,x::ys)
  in
    loop (xs, []) end;
val _ = clogBoolList (reverse [1,2,3,4]) [4,3,2,1];
val _ = clogBool ((reverse [true,false]) = [false, true]);

val op @ = append2;
infixr 5 @;
val _ = clogBoolList ([1,2,3] @ [4,5,6]) [1,2,3,4,5,6];