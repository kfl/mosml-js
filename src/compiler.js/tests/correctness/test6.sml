exception E;
fun f() = let exception E in raise E end;
val it = f() handle E => "WRONG: Top-level E is caught"
         | _ => "OK";
val elist = [E, Size];

exception E' = E;
val it = (raise E) handle E' => "OK";
val it = (raise E') handle E => "OK";

exception G of int;
val it = (raise G 20) handle G 10 => "WRONG: G 10"
                  | E    => "WRONG: E"
                  | G 20 => "OK";
