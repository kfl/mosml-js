prim_val clog_ : 'a -> unit = 1 "console.log";
fun clog x = clog_ x;
fun clogList [] = ()
  | clogList (x::xs) = (clog x; clogList xs);
fun clogBool true = clog "true"
  | clogBool false = clog "false";

prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;