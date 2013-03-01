local open Fnlib in

(* version string *)
val version = "2.10"

(* Integer ranges *)

val maxint_byte = 255
and minint_byte = 0
and maxint_short = 32767
and minint_short = ~32768
and maxint_int31 = 1073741823
and minint_int31 = ~1073741824
;

(* The default name for executable bytecode files. *)

val default_exec_name = "a.out";

(* Prompts *)

val toplevel_input_prompt = "- ";
val toplevel_output_prompt = "> ";
val toplevel_output_cont_prompt = "  ";
val toplevel_error_prompt = "! ";
val batch_output_prompt = "> ";
val batch_output_cont_prompt = "  ";
val batch_error_prompt = "! ";

(* Run-time values: MUST AGREE with runtime/mlvalues.h *)

val realTag     = 254;	
val stringTag   = 253;	
val refTag      = 250;	
val closureTag  = 249 ;
val maxBlockTag = closureTag-1;

(* Unit sets *)

(* The empty "none" set is defined in Mainc.sml, Mainl.sml, and Maint.sml *)

val reservedUnitNames = ["General", "Top", "Meta"];
val pervasiveOpenedUnits = ["General"];

val fulllib = ["Option", "List", "ListPair", "Strbase", "Char", "String",
	       "StringCvt", "TextIO", "BasicIO", "Vector",
	       "Array", "VectorSlice", "ArraySlice", "Misc", "Substring",
	       "Bool", "Int", "Real", "Math",
	       "Word", "Word8", "Word8Vector", "Word8Array", 
	       "Word8VectorSlice", "Word8ArraySlice", "Byte",
	       "BinIO", "CharVector", "CharArray",
	       "CharVectorSlice", "CharArraySlice",
	       "Time", "Timer", "Date", "Path",
	       "FileSys", "Process", "OS", 
	       "Mosml", "PP", "CommandLine"]

val preloadedUnitSets = [
  ("default",  ["Option", "List", "Strbase", "Char", "String",
		"StringCvt", "TextIO", "BasicIO", "Vector",
		"Array", "Misc"]),
  ("full",     fulllib),
  ("sml90",    ["Option", "List", "Strbase", "Char", "String",
                "StringCvt", "TextIO", "BasicIO", "Vector",
		"Array", "Misc", "SML90"]),
  ("nj93",     ["Option", "List", "Strbase", "Char", "String",
		"StringCvt", "TextIO", "BasicIO", "NJ93", "Vector",
		"Array", "Misc"])
];

val preopenedPreloadedUnitSets = [
  ("default",  ["Misc"]),
  ("full",     ["Misc"]),
  ("sml90",    ["Misc", "SML90"]),
  ("nj93",     ["Misc", "NJ93"])
];



fun normalizedFileName s = s;
fun normalizedUnitName s = s;

(* To translate escape sequences *)

val char_for_backslash = fn
(* *)    #"n" => #"\010"
(* *)  | #"r" => #"\013"
(* *)  | #"a" => #"\007"
(* *)  | #"b" => #"\008"
(* *)  | #"t" => #"\009"
(* *)  | #"v" => #"\011"
(* *)  | #"f" => #"\012"
(* *)  | c => c
;

end;
