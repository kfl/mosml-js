prim_val clog_ : 'a -> unit = 1 "console.log"

fun clog x = clog_ x

fun clogList [] = ()
  | clogList (x::xs) = (clog x; clogList xs)

fun clogBool true = clog "true"
  | clogBool false = clog "false"