(* 
app use ["Dyn.sig", "Dyn-ref.sml", "Dyntest.sml"];
app use ["Dyn.sig", "Dyn.sml", "Dyntest.sml"];
*)

val int = Dyn.new()
val str = Dyn.new()
val bool = Dyn.new()

val ds = [#into int 42, #into str "fourty two", #into int 23, #into bool true]

val ints = List.mapPartial (#out int) ds
