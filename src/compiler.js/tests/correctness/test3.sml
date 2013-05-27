datatype 'a Tree =
    Lf
  | Br of 'a * 'a Tree * 'a Tree
;

prim_val clog_ : 'a -> unit = 1 "console.log"
fun clog x = clog_ x
fun clogTree Lf = "Lf"
  | clogTree (Br(a, left, right)) = ("Br("^a^", "^(clogTree left)^", "^(clogTree right)^")")


val t1 = Br("2", Br("1", Lf, Lf), Br("3", Lf, Lf));

fun foldTree f u Lf = u
  | foldTree f u (Br(a, left, right)) =
      f a (foldTree f u left) (foldTree f u right)
;

fun revBranch a left right = Br(a, right, left);

local
    val reflect = foldTree revBranch Lf;
in
    val refl_t1 = clog (clogTree (reflect t1));
end
