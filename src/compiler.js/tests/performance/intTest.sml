prim_val ctimebegin_ : string -> unit = 1 "console.time";
prim_val ctimeend_ : string -> unit = 1 "console.timeEnd";
fun ctimeBegin n = ctimebegin_ n;
fun ctimeEnd n = ctimeend_ n;

fun recAdd 100000000 = ()
  | recAdd x = recAdd (x+1)
  
  
fun recSub 0 = ()
  | recSub x = recSub (x-1)


val it = ctimeBegin "Add1";
val it = recAdd 0;
val it = ctimeEnd "Add1";
val it = ctimeBegin "Add2";
val it = recAdd 0;
val it = ctimeEnd "Add2";
val it = ctimeBegin "Add3";
val it = recAdd 0;
val it = ctimeEnd "Add3";
val it = ctimeBegin "Add4";
val it = recAdd 0;
val it = ctimeEnd "Add4";
val it = ctimeBegin "Add5";
val it = recAdd 0;
val it = ctimeEnd "Add5";
val it = ctimeBegin "Add6";
val it = recAdd 0;
val it = ctimeEnd "Add6";
val it = ctimeBegin "Add7";
val it = recAdd 0;
val it = ctimeEnd "Add7";
val it = ctimeBegin "Add8";
val it = recAdd 0;
val it = ctimeEnd "Add8";
val it = ctimeBegin "Add9";
val it = recAdd 0;
val it = ctimeEnd "Add9";
val it = ctimeBegin "Add10";
val it = recAdd 0;
val it = ctimeEnd "Add10";

val it = ctimeBegin "Sub1";
val it = recSub 1000000000;
val it = ctimeEnd "Sub1";
val it = ctimeBegin "Sub2";
val it = recSub 1000000000;
val it = ctimeEnd "Sub2";
val it = ctimeBegin "Sub3";
val it = recSub 1000000000;
val it = ctimeEnd "Sub3";
val it = ctimeBegin "Sub4";
val it = recSub 1000000000;
val it = ctimeEnd "Sub4";
val it = ctimeBegin "Sub5";
val it = recSub 1000000000;
val it = ctimeEnd "Sub5";
val it = ctimeBegin "Sub6";
val it = recSub 1000000000;
val it = ctimeEnd "Sub6";
val it = ctimeBegin "Sub7";
val it = recSub 1000000000;
val it = ctimeEnd "Sub7";
val it = ctimeBegin "Sub8";
val it = recSub 1000000000;
val it = ctimeEnd "Sub8";
val it = ctimeBegin "Sub9";
val it = recSub 1000000000;
val it = ctimeEnd "Sub9";
val it = ctimeBegin "Sub10";
val it = recSub 1000000000;
val it = ctimeEnd "Sub10";

