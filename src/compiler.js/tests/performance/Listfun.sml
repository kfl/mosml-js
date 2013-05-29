(* A few list functions written in different styles *)

fun len lst =
    let fun loop acc [] = acc
          | loop acc (_::xs) = loop (acc+1) xs
    in loop 0 lst end

fun lenw lst =
    let val cnt = ref 0
        val next = ref lst
    in  while not(null(!next)) do (
            cnt := !cnt + 1
          ; next := tl(!next)
        )
      ; !cnt
    end

fun lenr [] = 0
  | lenr (_::xs) = 1 + lenr xs 



fun tab f n =
    let fun loop acc 0 = f 0 :: acc
          | loop acc n = loop (f n :: acc) (n-1)
    in  loop [] (n-1)
    end

fun tabw f n =
    let val i = ref(n-1)
        val acc = ref []
    in  while !i >= 0 do (
            acc := f(!i) :: !acc
          ; i := !i - 1
        )
      ; !acc
    end

fun map _ [] = []
  | map f (x::xs) = f x :: map f xs 

fun mapc f xs =
    let fun cps [] k = k[]
          | cps (x::xs) k = cps xs (fn res => k (f x :: res))
    in  cps xs (fn x => x)
    end

fun mapcw f xs =
    let val k = ref(fn x => x)
        val next = ref xs
    in  while not(null(!next)) do (
            k := let val c = !k val x = hd(!next) in (fn res => c (f x :: res)) end
          ; next := tl(!next)
        ) 
      ; !k []
    end

fun mapw f xs =
    let val res = ref[]
        val next = ref xs
    in  while not(null(!next)) do (
            res := let val x = hd(!next) in f x :: (!res) end
          ; next := tl(!next)
        ) 
      ; rev (!res)
    end

datatype ('a, 'b) action = RET  of 'a
                         | NEXT of ('b -> ('a, 'b) action) * 'b 
fun continue (NEXT _) = true
  | continue _ = false

fun doAction (NEXT(f, x)) = f x
fun getResult (RET x) = x

fun trampoline f x =
    let val next = ref (f x)
    in  while continue(!next) do
              next := doAction(!next)
      ; getResult(!next)
    end

fun mapct f xs =
    let fun next ([], k) = RET (NEXT(k, []))
          | next (x::xs, k) = NEXT(next, (xs, fn res => NEXT(k, f x :: res)))
    in  trampoline (trampoline next) (xs, fn x => RET x)
    end

(* Helper functions for repating and timing computations *)

fun repeat n f x =
    let fun loop 0 = f x
          | loop n = (f x; loop(n-1))
    in  loop(n-1) end 

fun timerep n f = Mosml.time (repeat n f)
