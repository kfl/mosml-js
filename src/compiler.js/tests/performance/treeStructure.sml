prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;

fun repeat n f x =
    let val n = ref n
    in  while !n > 0 do (n := !n-1; f x) end;
fun timerep n f s = repeat n (fn x => (ctimeBegin(s);f x; ctimeEnd(s)));

datatype Tree =
    Node of int * Tree * Tree
  | Leaf

fun rev xs =
  let
    fun revAcc [] ys = ys
    | revAcc (x::xs) ys = revAcc xs (x::ys)
  in
    revAcc xs []
  end

fun intList 0 = [0]
  | intList n = n :: intList (n-1)

fun genTree [] = Leaf
  | genTree (x::xs) = Node (x, genTree xs, genTree (rev xs))

fun traverseTree (Leaf, _) = 0
  | traverseTree (Node(x, left, right), n) =
    if n = x
    then 1 + traverseTree (left, n) + traverseTree (right, n)
    else traverseTree (left, n) + traverseTree (right, n)

val _ = timerep 100 genTree "genTree" (intList 19)

val tree = genTree (intList 19)
val _ = timerep 100 traverseTree "traverseTree" (tree, 0)