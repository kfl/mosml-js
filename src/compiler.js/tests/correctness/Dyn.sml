structure Dyn :> Dyn =
struct
  type t = exn
  fun new () = 
      let exception Dyn of 'a
      in  { into = fn x => Dyn x, 
            out  = fn Dyn x => SOME x
                    | _     => NONE }
      end
end
