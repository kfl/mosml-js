val it = let val r = ref []
in r := [7]; !r end;

fun Id x = let val r = ref x in !r end;
fun Id' x = Id Id x;

fun reverse l =
  let val res = ref []
      fun loop [] = !res
        | loop (hd :: tl) = (res:= hd::(!res); loop tl)
  in loop l end;

val it = reverse [1,2,3];
val it = reverse [true,false];

val f = fn x as ref u => (x := 666; (x, u));
val it = f (ref 99);

val it = let fun plus x y = x+y in plus 3 5 end;
