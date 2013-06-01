prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;

fun repeat n f x =
  let 
    fun loop 0 = f x
      | loop n = (f x; loop(n-1))
  in 
    loop(n-1) 
  end 
fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)))


fun realAdd x =
  let
    val addRef = ref 0.0;
  in
    while !addRef < x do (addRef := !addRef+1.0; 33.3334+9.992)
  end

fun realSub x =
  let
    val subRef = ref 0.0;
  in
    while !subRef < x do (subRef := !subRef+1.0; 33.3334-9.992)
  end
  

val it = timerep 100 realAdd "realAdd" 10000000.0
val it = timerep 100 realSub "realSub" 10000000.0




