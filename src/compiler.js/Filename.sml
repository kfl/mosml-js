(* filename.mlp *)

(* open CharVector; *)

fun extract arg = Substring.string(Substring.extract arg)

fun check_suffix name suff =
  let val name_len = size name
      val suff_len = size suff
  in
    name_len >= suff_len andalso
    extract(name, name_len - suff_len, SOME suff_len) = suff
  end;

fun chop_suffix name suff =
  extract(name, 0, SOME (size name - size suff))
;

val current_dir_name = ".";

fun concat dirname filename =
  let val len = size dirname
      val x   = if len = 0 then "/" else extract(dirname, len-1, SOME 1)
  in
    case x of
        "/"   => dirname ^ filename
      | _     => dirname ^ "/" ^ filename
  end;

fun is_absolute n =
  let val len = size n in
     (len >= 1 andalso extract(n, 0, SOME 1) = "/")    orelse
     (len >= 2 andalso extract(n, 0, SOME 2) = "./")   orelse
     (len >= 3 andalso extract(n, 0, SOME 3) = "../")
  end;

fun slash_pos s =
  let fun pos i =
    if i < 0 then NONE else
    case extract(s, i, SOME 1) of
        "/"  => SOME i
      | _    => pos (i - 1)
  in pos (size s - 1) end
;

fun basename name =
  case slash_pos name of
      SOME p => 
        extract(name, p+1, NONE)
    | NONE   => name
;

fun dirname name =
  if name = "/" then name else
  case slash_pos name of
      SOME p  => extract(name, 0, SOME p)
    | NONE    => "."
;


