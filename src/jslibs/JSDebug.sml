prim_val clog_ : 'a -> unit = 1 "console.log"

fun clog x = clog_ x

fun cloglist [] = ()
  | cloglist (x::xs) = (clog x; cloglist xs)