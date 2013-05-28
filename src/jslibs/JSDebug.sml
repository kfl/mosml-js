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

fun repeat n f x =
    let fun loop 0 = f x
          | loop n = (f x; loop(n-1))
    in  loop(n-1) end;

fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)));