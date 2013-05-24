fun fib n =
  let fun fibcalc 0.0 x y = x
      | fibcalc n x y = fibcalc (n-1.0) y (y+x)
  in
    fibcalc n 0.0 1.0
  end;

val a = fib 10.0
val b = fib 50.0
val c = fib 80.0
