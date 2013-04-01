open JSInstruct Const Buffcode BasicIO Nonstdio;

fun outConst (JSNUMscon i) = out i
  | outConst (JSSTRscon s) = out "\""^s^"\""
;
fun outList [] = ()
  | outList (JSLISTsc s) :: [] = (out "[";outList s;out "]")
  | outList (JSATOMsc s) :: [] = outConst s
  | outList (JSLISTsc s) :: ss = (out "[";outList s;out "]"); out ","; outList ss
  | outList (JSATOMsc s) :: ss = outConst s; out ","; outList ss
;

(*Emit the given phrase in abstract js language defined in JSInstruct.sml.*)
fun emit jsinstr =
  case jsinstr of
    JSAdd(a,b) => (emit a; out "+"; emit b)
  | JSConst(JSATOMsc k) => outConst k
  | JSConst(JSLISTsc l) => (out "[";outList l;out "]")
  | JSGetVar(_,qual) => out qual
  | JSSetVar((_,qual), js) => (out qual^"="; emit [js]; out "; ")
  | _ => out " Error! "
    
  and emitList jsinstrlist = ()
;

val abs_out_position = ref 0;

val compiled_phrase_index = ref ([] : compiled_phrase list);

fun start_emit_phrase os =
(
  output_binary_int os 0;
  abs_out_position := 4;
  compiled_phrase_index := []
);

fun emitPhrase os (ajs : JSInstruction) =
(
  init_out_code();
  Labels.reset_label_table();

  emit ajs;
  buff_output os (!out_buffer) 0 (!out_position);
  
  compiled_phrase_index :=
    { cph_pos   = !abs_out_position,
      cph_len   = !out_position,
      cph_reloc = get_reloc_info()}
        :: !compiled_phrase_index;
  abs_out_position := !abs_out_position + !out_position
);

fun end_emit_phrase
  excRenList valRenList sigStamp mentions os =
(
  output_value os
    { cu_phrase_index = !compiled_phrase_index,
      cu_exc_ren_list = excRenList,
      cu_val_ren_list = valRenList,
      cu_sig_stamp = sigStamp,
      cu_mentions = mentions };
  compiled_phrase_index := [];
  seek_out os 0;
  output_binary_int os (!abs_out_position)
);