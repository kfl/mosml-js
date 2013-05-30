prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;
fun repeat n f x =
    let val n = ref n
    in  while !n > 0 do (n := !n-1; f x) end;
fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)));

fun recAdd 9500 = ()
  | recAdd x = recAdd (x+1)

fun recSub 0 = ()
  | recSub x = recSub (x-1)

val it = timerep 100 recAdd "recAdd" 0
val it = timerep 100 recSub "recSub" 9500