prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;
fun repeat n f x =
    let val n = ref n
    in  while !n > 0 do (n := !n-1; f x) end;
fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)));

fun intAddRec 9500 = ()
  | intAddRec x = recAdd (x+1)

fun intSubRec 0 = ()
  | intSubRec x = recSub (x-1)

val it = timerep 100 intAddRec "intAddRec" 0
val it = timerep 100 intSubRec "intSubRec" 9500