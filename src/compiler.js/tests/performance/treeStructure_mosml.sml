fun repeat n f x =
    let fun loop 0 = Mosml.time f x
          | loop n = (Mosml.time f x; loop(n-1))
    in  loop(n-1) end

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

val tree = genTree (intList 19)
val _ = repeat 10 genTree (intList 19)
val _ = repeat 10 traverseTree (tree, 0)