signature Dyn =
sig
    type t
    val new : unit -> {into : 'a -> t, out : t -> 'a option }
end
