prim_val clog_ : 'a -> unit = 1 "console.log"

fun clog x = clog_ x

fun clogBool true = clog "true"
  | clogBool false = clog "false"
  

val _ = let val r = ref []
in r := [7]; clogBool ((!r)=[7])  end;

fun Id x = let val r = ref x in !r end;
fun Id' x = Id Id x;

fun reverse l =
  let val res = ref []
      fun loop [] = !res
        | loop (hd :: tl) = (res:= hd::(!res); loop tl)
  in loop l end;

val _ = clogBool ((reverse [1,2,3])=[3,2,1]) ;
val _ = reverse [true,false];

val f = fn x as ref u => (x := 666; (x, u));
val it = f (ref 99);
val _ = clogBool(it = (ref 666,99));

val _ = let fun plus x y = x+y in clogBool ((plus 3 5) = 8) end;
