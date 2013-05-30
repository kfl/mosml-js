prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;

fun repeat n f x =
    let val n = ref n
    in  while !n > 0 do (n := !n-1; f x) end;
fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)));

fun naiveFib 0.0 = 1.0
  | naiveFib 1.0 = 1.0
  | naiveFib n = naiveFib (n-1.0) + naiveFib (n-2.0)

val _ = timerep 100 naiveFib "naiveFib" 35.0