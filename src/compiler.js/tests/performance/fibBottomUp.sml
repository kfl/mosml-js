prim_val clog_ : 'a -> unit = 1 "console.log"
prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;
fun clog x = clog_ x

fun repeat n f x =
    let val n = ref n
    in  while !n > 0 do (n := !n-1; f x) end;
fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)));

fun fib n =
  let fun fibcalc 0.0 x y = x
      | fibcalc n x y = fibcalc (n-1.0) y (y+x)
  in
    fibcalc n 0.0 1.0
  end;

val _ = timerep 100 fib "fibBottomUp" 1000.0