prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;

prim_val clog_ : 'a -> unit = 1 "console.log";
fun clog x = clog_ x;
fun length xs =
    let fun acc []      k = k
          | acc (x::xr) k = acc xr (k+1)
    in acc xs 0 end;


local
  fun append [] ys = ys
    | append (x::xs) ys = x :: append xs ys
in
  fun xs @ [] = xs
    | xs @ ys = append xs ys
end


fun repeat n f x =
  let 
    fun loop 0 = f x
      | loop n = (f x; loop(n-1))
  in 
    loop(n-1) 
  end 
fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)))


fun listPrepend x =
  let
    val addRef = ref 0.0;
    val listRef = ref []
  in
    while !addRef < x do 
      (addRef := !addRef+1.0;
       listRef := 1::(!listRef)
      )
  end

fun listAppend x =
  let
    val addRef = ref 0.0;
    val listRef = ref []
  in
    while !addRef < x do 
      (addRef := !addRef+1.0;
       listRef := (!listRef)@[1]
      )
  end


val it = timerep 10 listPrepend "listPrepend" 100000.0
val it = timerep 10 listAppend "listPrepend" 1000.0


