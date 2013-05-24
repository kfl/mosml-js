structure Dyn :> Dyn =
struct
  type t = unit -> unit
  fun new () = 
      let val r = ref NONE
      in  { into = fn x => fn () => r := SOME x,
            out  = fn f => (f(); !r before r := NONE)}
      end
end

(* Implementation of `before`:

infix bef
fun x bef y = x
*)