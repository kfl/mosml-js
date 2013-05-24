
(* Moscow ML handle the first two tests correct. *)
fun csnd x y = y

local val r = ref "WRONG" in
val test1 = csnd (r := "OK"; ()) (!r);
end

local 
    val r = ref "WRONG"
    val fr = ref (fn () => (r := "OK"; csnd))
in 
val test2 = (!fr ()) () !r;
end


(* But it's downhill from here... *)
local
    exception Right and Wrong
    fun f y = (raise Right; fn x => x)
in
val test3 = (f 7 (raise Wrong))
            handle Right => "OK"
                 | Wrong => "WRONG" 
                                
end

local
    val r = ref "WRONG"
    fun f y = #2 (r := "OK", fn x => x)
in
val test4 = (f 7 (!r), !r)
end
