prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;
fun repeat n f x =
    let val n = ref n
    in  while !n > 0 do (n := !n-1; f x) end;
fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)));

fun intAdd x =
  let
    val addRef = ref 0.0;
  in
    while !addRef < x do (addRef := !addRef+1.0; 10+10)
  end

fun intSub x =
  let
    val subRef = ref 0.0;
  in
    while !subRef < x do (subRef := !subRef+1.0; 10-1)
  end


val it = timerep 100 intAdd "intAdd" 1000000.0
val it = timerep 100 intSub "intSub" 1000000.0




