local open Obj Lexing in


open Fnlib Memory Config Mixture Const Parser;

(* For Quote/Antiquote --- object language embedding. *)

val quotation = ref false

datatype lexingMode =
    NORMALlm
  | QUOTElm
  | ANTIQUOTElm

val lexingMode = ref NORMALlm

val parCount = Stack.new() : int Stack.t

fun resetLexerState() =
(
  lexingMode := NORMALlm;
  Stack.clear parCount
)

(* For nesting comments *)

val comment_depth = ref 0

(* The table of keywords *)

val keyword_table = (Hasht.new 53 : (string,token) Hasht.t)

val () =
List.app (fn (str,tok) => Hasht.insert keyword_table str tok)
[
  ("abstype",      ABSTYPE),
  ("and",          AND),
  ("andalso",      ANDALSO),
  ("as",           AS),
  ("case",         CASE),
  ("datatype",     DATATYPE),
  ("do",           DO),
  ("else",         ELSE),
  ("eqtype",       EQTYPE),
  ("end",          END),
  ("exception",    EXCEPTION),
  ("fn",           FN),
  ("fun",          FUN),
  ("functor",      FUNCTOR),
  ("handle",       HANDLE),
  ("if",           IF),
  ("in",           IN),
  ("include",      INCLUDE),
  ("infix",        INFIX),
  ("infixr",       INFIXR),
  ("let",          LET),
  ("local",        LOCAL),
  ("nonfix",       NONFIX),
  ("of",           OF),
  ("op",           OP),
  ("open",         OPEN),
  ("orelse",       ORELSE),
  ("prim_eqtype",  PRIM_EQTYPE),
  ("prim_EQtype",  PRIM_REFTYPE),
  ("prim_type",    PRIM_TYPE),
  ("prim_val",     PRIM_VAL),
  ("raise",        RAISE),
  ("rec",          REC),
  ("sharing",      SHARING),
  ("sig",          SIG),
  ("signature",    SIGNATURE),
  ("struct",       STRUCT),
  ("structure",    STRUCTURE),
  ("then",         THEN),
  ("type",         TYPE),
  ("val",          VAL),
  ("where",        WHERE),
  ("while",        WHILE),
  ("with",         WITH),
  ("withtype",     WITHTYPE),
  ("#",            HASH),
  ("->",           ARROW),
  ("|",            BAR),
  (":>",           COLONGT),
  (":",            COLON),
  ("=>",           DARROW),
  ("=",            EQUALS),
  ("*",            STAR)
]

fun mkKeyword lexbuf =
  let val s = getLexeme lexbuf in
    Hasht.find keyword_table s
    handle Subscript => ID s
  end

val savedLexemeStart = ref 0

val initial_string_buffer = CharArray.array(256, #"\000")
val string_buff = ref initial_string_buffer
val string_index = ref 0

fun reset_string_buffer() =
(
  string_buff := initial_string_buffer;
  string_index := 0;
  ()
)

fun store_string_char c =
  let open CharArray
      val len = length (!string_buff)
  in
    if !string_index >= len then
      let val new_buff = array(len * 2, #"\000") in
        copy { src = !string_buff, dst = new_buff, di = 0 };
        string_buff := new_buff
      end
    else ();
    update(!string_buff, !string_index, c);
    incr string_index
  end

fun get_stored_string() =
  let open CharArraySlice
      val s = vector(slice(!string_buff, 0, SOME (!string_index)))
  in
    string_buff := initial_string_buffer;
    s
  end

(* cvr: NOTE normalizeUnitName done elsewhere now *)
fun splitQualId s =
  let open CharVectorSlice
      val len' = size s
      fun parse i n acc =
        if n >= len' then
	  vector(slice(s, i, SOME (len' - i))) :: acc
        else if CharVector.sub(s, n) = #"." then
          parse (n+1) (n+1) (vector(slice(s, i, SOME (n - i)))::acc)
        else
          parse i (n+1) acc
  in parse 0 0 [] end



fun mkQualId lexbuf =
  let val  id = splitQualId(getLexeme lexbuf) in
    if id = ["*"] then
      QUAL_STAR { qual="", id=id }
    else
      QUAL_ID   { qual="", id=id }
  end

fun charCodeOfDecimal lexbuf i =
  100 * (Char.ord(getLexemeChar lexbuf i) - 48) +
   10 * (Char.ord(getLexemeChar lexbuf (i+1)) - 48) +
        (Char.ord(getLexemeChar lexbuf (i+2)) - 48)


fun charCodeOfHexadecimal lexbuf i =
    let fun hexval c = 
	    if #"0" <= c andalso c <= #"9" then Char.ord c - 48
	    else (Char.ord c - 55) mod 32;
    in 
       4096 * hexval(getLexemeChar lexbuf (i+1)) +
        256 * hexval(getLexemeChar lexbuf (i+2)) +
         16 * hexval(getLexemeChar lexbuf (i+3)) +
              hexval(getLexemeChar lexbuf (i+4)) 
    end

fun lexError msg lexbuf =
(
  resetLexerState();
  raise LexicalError(msg, getLexemeStart lexbuf, getLexemeEnd lexbuf)
)

fun constTooLarge msg lexbuf =
(
  resetLexerState();
  lexError (msg ^ " constant is too large") lexbuf
)

prim_val sml_word_of_string    : string -> word = 1 "sml_word_of_dec"
prim_val sml_word_of_hexstring : string -> word = 1 "sml_word_of_hex"

fun notTerminated msg lexbuf =
(
  resetLexerState();
  raise LexicalError (msg ^ " not terminated",
                      !savedLexemeStart, getLexemeEnd lexbuf)
)

fun skipString msg skip lexbuf =
  let
    val pos1 = getLexemeStart lexbuf
    val pos2 = getLexemeEnd lexbuf
  in
    skip lexbuf;
    resetLexerState();
    raise (LexicalError(msg, pos1, pos2))
  end

fun scanString scan lexbuf =
(
  reset_string_buffer();
  savedLexemeStart := getLexemeStart lexbuf;
  scan lexbuf;
  setLexStartPos lexbuf (!savedLexemeStart - getLexAbsPos lexbuf)
)


fun action_71 lexbuf = (
 case !lexingMode of
            NORMALlm =>
              TokenN lexbuf
          | QUOTElm =>
              (scanString Quotation lexbuf;
               case !lexingMode of
                   NORMALlm =>
                     QUOTER (get_stored_string())
                 | ANTIQUOTElm =>
                     QUOTEM (get_stored_string())
                 | QUOTElm =>
                     fatalError "Token")
          | ANTIQUOTElm =>
              AntiQuotation lexbuf
      )
and action_70 lexbuf = (
 lexError "this will be never called!" lexbuf )
and action_69 lexbuf = (
 if !quotation then TokenIdQ lexbuf else TokenId lexbuf )
and action_68 lexbuf = (
 EOF )
and action_67 lexbuf = (
 SEMICOLON )
and action_66 lexbuf = (
 if not(Stack.null parCount) then
          let val count = Stack.pop parCount - 1 in
            if count = 0 then
              (lexingMode := QUOTElm; Token lexbuf)
            else
              (Stack.push count parCount; RPAREN)
          end
        else
          RPAREN
      )
and action_65 lexbuf = (
 if not(Stack.null parCount) then
         Stack.push (Stack.pop parCount + 1) parCount
       else ();
       LPAREN
     )
and action_64 lexbuf = (
 RBRACKET )
and action_63 lexbuf = (
 HASHLBRACKET )
and action_62 lexbuf = (
 LBRACKET )
and action_61 lexbuf = (
 RBRACE )
and action_60 lexbuf = (
 LBRACE )
and action_59 lexbuf = (
 DOTDOTDOT )
and action_58 lexbuf = (
 COMMA )
and action_57 lexbuf = (
 UNDERBAR )
and action_56 lexbuf = (
 scanString String lexbuf;
        let val s = get_stored_string() in
          if size s <> 1 then
            lexError "ill-formed character constant" lexbuf
          else ();
          CHAR (CharVector.sub(s, 0))
        end )
and action_55 lexbuf = (
 scanString String lexbuf;
        STRING (get_stored_string())
      )
and action_54 lexbuf = (
 REAL (sml_float_of_string (getLexeme lexbuf))
                  handle Fail _ => constTooLarge "real" lexbuf
                )
and action_53 lexbuf = (
 WORD (sml_word_of_hexstring(getLexeme lexbuf))
                  handle Fail _ => constTooLarge "word" lexbuf
                )
and action_52 lexbuf = (
 WORD (sml_word_of_string(getLexeme lexbuf))
                  handle Fail _ => constTooLarge "word" lexbuf
                )
and action_51 lexbuf = (
 NEGINT    (sml_hex_of_string(getLexeme lexbuf))
                  handle Fail _ => constTooLarge "integer" lexbuf
                )
and action_50 lexbuf = (
 NEGINT    (sml_int_of_string(getLexeme lexbuf))
                  handle Fail _ => constTooLarge "integer" lexbuf
                )
and action_49 lexbuf = (
 NZPOSINT2 (sml_int_of_string(getLexeme lexbuf))
                  handle Fail _ => constTooLarge "integer" lexbuf
                )
and action_48 lexbuf = (
 ZPOSINT2  (sml_int_of_string(getLexeme lexbuf))
                  handle Fail _ => constTooLarge "integer" lexbuf
                )
and action_47 lexbuf = (
 NZDIGIT   (sml_int_of_string(getLexeme lexbuf)) )
and action_46 lexbuf = (
 ZDIGIT 0 )
and action_45 lexbuf = (
 TYVAR   (getLexeme lexbuf) )
and action_44 lexbuf = (
 lexError "unmatched comment bracket" lexbuf )
and action_43 lexbuf = (
 savedLexemeStart := getLexemeStart lexbuf;
        comment_depth := 1; Comment lexbuf; TokenN lexbuf
      )
and action_42 lexbuf = (
 TokenN lexbuf )
and action_41 lexbuf = (
 lexError "ill-formed token" lexbuf )
and action_40 lexbuf = (
 mkQualId lexbuf )
and action_39 lexbuf = (
 mkKeyword lexbuf )
and action_38 lexbuf = (
 lexError "ill-formed token" lexbuf )
and action_37 lexbuf = (
 lexingMode := QUOTElm; QUOTEL )
and action_36 lexbuf = (
 mkQualId lexbuf )
and action_35 lexbuf = (
 mkKeyword lexbuf )
and action_34 lexbuf = (
 Comment lexbuf )
and action_33 lexbuf = (
 notTerminated "comment" lexbuf )
and action_32 lexbuf = (
 (decr comment_depth;
         if !comment_depth > 0 then Comment lexbuf else ()) )
and action_31 lexbuf = (
 (incr comment_depth; Comment lexbuf) )
and action_30 lexbuf = (
 (store_string_char(getLexemeChar lexbuf 0);
         String lexbuf) )
and action_29 lexbuf = (
 skipString "invalid character in string" SkipString lexbuf )
and action_28 lexbuf = (
 skipString "newline not permitted in string" SkipString lexbuf )
and action_27 lexbuf = (
 notTerminated "string" lexbuf )
and action_26 lexbuf = (
 skipString "ill-formed escape sequence" SkipString lexbuf )
and action_25 lexbuf = (
 let val code = charCodeOfHexadecimal lexbuf 1 in
          if code >= 256 then
            skipString "character code is too large" SkipString lexbuf
          else ();
          store_string_char(Char.chr code);
          String lexbuf
        end )
and action_24 lexbuf = (
 let val code = charCodeOfDecimal lexbuf 1 in
          if code >= 256 then
            skipString "character code is too large" SkipString lexbuf
          else ();
          store_string_char(Char.chr code);
          String lexbuf
        end )
and action_23 lexbuf = (
 store_string_char(
          Char.chr(Char.ord(getLexemeChar lexbuf 2) - 64));
        String lexbuf )
and action_22 lexbuf = (
 String lexbuf )
and action_21 lexbuf = (
 store_string_char(char_for_backslash(getLexemeChar lexbuf 1));
        String lexbuf )
and action_20 lexbuf = (
 () )
and action_19 lexbuf = (
 SkipString lexbuf )
and action_18 lexbuf = (
 notTerminated "string" lexbuf )
and action_17 lexbuf = (
 SkipString lexbuf )
and action_16 lexbuf = (
 SkipString lexbuf )
and action_15 lexbuf = (
 () )
and action_14 lexbuf = (
 (store_string_char(getLexemeChar lexbuf 0);
         Quotation lexbuf) )
and action_13 lexbuf = (
 skipString "invalid character in quotation" SkipQuotation lexbuf )
and action_12 lexbuf = (
 lexingMode := NORMALlm;
        notTerminated "quotation" lexbuf
      )
and action_11 lexbuf = (
 (store_string_char(getLexemeChar lexbuf 0);
         Quotation lexbuf) )
and action_10 lexbuf = (
 Quotation lexbuf )
and action_9 lexbuf = (
 lexingMode := ANTIQUOTElm )
and action_8 lexbuf = (
 lexingMode := NORMALlm )
and action_7 lexbuf = (
 SkipQuotation lexbuf )
and action_6 lexbuf = (
 lexingMode := NORMALlm;
        notTerminated "quotation" lexbuf
      )
and action_5 lexbuf = (
 lexingMode := NORMALlm )
and action_4 lexbuf = (
 
        skipString "ill-formed antiquotation" SkipQuotation lexbuf
      )
and action_3 lexbuf = (
 lexingMode := NORMALlm;
        notTerminated "antiquotation" lexbuf
      )
and action_2 lexbuf = (
 lexingMode := NORMALlm;
        lexError "antiquotation is missing" lexbuf
      )
and action_1 lexbuf = (
 Stack.push 1 parCount; lexingMode := NORMALlm;
        TokenN lexbuf
      )
and action_0 lexbuf = (
 lexingMode := QUOTElm;
        mkKeyword lexbuf
      )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_120 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_120 lexbuf
 else case currChar of
    #"!" => state_118 lexbuf
 |  #"&" => state_118 lexbuf
 |  #"%" => state_118 lexbuf
 |  #"$" => state_118 lexbuf
 |  #"#" => state_118 lexbuf
 |  #"+" => state_118 lexbuf
 |  #"*" => state_118 lexbuf
 |  #"-" => state_118 lexbuf
 |  #"/" => state_118 lexbuf
 |  #":" => state_118 lexbuf
 |  #"@" => state_118 lexbuf
 |  #"?" => state_118 lexbuf
 |  #">" => state_118 lexbuf
 |  #"=" => state_118 lexbuf
 |  #"<" => state_118 lexbuf
 |  #"\\" => state_118 lexbuf
 |  #"|" => state_118 lexbuf
 |  #"~" => state_118 lexbuf
 |  #"`" => action_2 lexbuf
 |  #"(" => action_1 lexbuf
 |  #"\^Z" => action_3 lexbuf
 |  #"\^@" => action_3 lexbuf
 |  _ => action_4 lexbuf
 end)
and state_1 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"`" => action_5 lexbuf
 |  #"\^Z" => action_6 lexbuf
 |  #"\^@" => action_6 lexbuf
 |  _ => action_7 lexbuf
 end)
and state_2 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\^A" andalso currChar <= #"\b" then  action_13 lexbuf
 else if currChar >= #"\^N" andalso currChar <= #"\^Y" then  action_13 lexbuf
 else case currChar of
    #"\f" => action_13 lexbuf
 |  #"\v" => action_13 lexbuf
 |  #"\127" => action_13 lexbuf
 |  #"\255" => action_13 lexbuf
 |  #"\n" => action_11 lexbuf
 |  #"\t" => action_11 lexbuf
 |  #"`" => action_8 lexbuf
 |  #"^" => action_9 lexbuf
 |  #"\^Z" => action_12 lexbuf
 |  #"\r" => action_10 lexbuf
 |  #"\^@" => action_12 lexbuf
 |  _ => action_14 lexbuf
 end)
and state_3 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\\" => state_99 lexbuf
 |  #"\"" => action_15 lexbuf
 |  #"\^Z" => action_18 lexbuf
 |  #"\^@" => action_18 lexbuf
 |  _ => action_19 lexbuf
 end)
and state_4 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"\^A" andalso currChar <= #"\t" then  action_29 lexbuf
 else if currChar >= #"\^N" andalso currChar <= #"\^Y" then  action_29 lexbuf
 else case currChar of
    #"\f" => action_29 lexbuf
 |  #"\v" => action_29 lexbuf
 |  #"\127" => action_29 lexbuf
 |  #"\255" => action_29 lexbuf
 |  #"\n" => action_28 lexbuf
 |  #"\r" => action_28 lexbuf
 |  #"\\" => state_81 lexbuf
 |  #"\"" => action_20 lexbuf
 |  #"\^Z" => action_27 lexbuf
 |  #"\^@" => action_27 lexbuf
 |  _ => action_30 lexbuf
 end)
and state_5 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => state_72 lexbuf
 |  #"(" => state_71 lexbuf
 |  #"\^Z" => action_33 lexbuf
 |  #"\^@" => action_33 lexbuf
 |  _ => action_34 lexbuf
 end)
and state_6 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_61 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_61 lexbuf
 else case currChar of
    #"!" => state_60 lexbuf
 |  #"&" => state_60 lexbuf
 |  #"%" => state_60 lexbuf
 |  #"$" => state_60 lexbuf
 |  #"#" => state_60 lexbuf
 |  #"+" => state_60 lexbuf
 |  #"*" => state_60 lexbuf
 |  #"-" => state_60 lexbuf
 |  #"/" => state_60 lexbuf
 |  #":" => state_60 lexbuf
 |  #"@" => state_60 lexbuf
 |  #"?" => state_60 lexbuf
 |  #">" => state_60 lexbuf
 |  #"=" => state_60 lexbuf
 |  #"<" => state_60 lexbuf
 |  #"\\" => state_60 lexbuf
 |  #"^" => state_60 lexbuf
 |  #"|" => state_60 lexbuf
 |  #"~" => state_60 lexbuf
 |  #"`" => action_37 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => action_38 lexbuf
 end)
and state_7 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_53 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_53 lexbuf
 else case currChar of
    #"!" => state_52 lexbuf
 |  #"&" => state_52 lexbuf
 |  #"%" => state_52 lexbuf
 |  #"$" => state_52 lexbuf
 |  #"#" => state_52 lexbuf
 |  #"+" => state_52 lexbuf
 |  #"*" => state_52 lexbuf
 |  #"-" => state_52 lexbuf
 |  #"/" => state_52 lexbuf
 |  #":" => state_52 lexbuf
 |  #"@" => state_52 lexbuf
 |  #"?" => state_52 lexbuf
 |  #">" => state_52 lexbuf
 |  #"=" => state_52 lexbuf
 |  #"<" => state_52 lexbuf
 |  #"\\" => state_52 lexbuf
 |  #"^" => state_52 lexbuf
 |  #"`" => state_52 lexbuf
 |  #"|" => state_52 lexbuf
 |  #"~" => state_52 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => action_41 lexbuf
 end)
and state_8 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_69);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"1" andalso currChar <= #"9" then  state_21 lexbuf
 else case currChar of
    #"\n" => action_42 lexbuf
 |  #"\t" => action_42 lexbuf
 |  #"\r" => action_42 lexbuf
 |  #"\f" => action_42 lexbuf
 |  #" " => action_42 lexbuf
 |  #"\^@" => action_68 lexbuf
 |  #"\^Z" => action_68 lexbuf
 |  #"~" => state_28 lexbuf
 |  #"}" => action_61 lexbuf
 |  #"{" => action_60 lexbuf
 |  #"_" => action_57 lexbuf
 |  #"]" => action_64 lexbuf
 |  #"[" => action_62 lexbuf
 |  #";" => action_67 lexbuf
 |  #"0" => state_20 lexbuf
 |  #"." => state_19 lexbuf
 |  #"," => action_58 lexbuf
 |  #"*" => state_17 lexbuf
 |  #")" => action_66 lexbuf
 |  #"(" => state_15 lexbuf
 |  #"'" => state_14 lexbuf
 |  #"#" => state_13 lexbuf
 |  #"\"" => action_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_71);
 let val currChar = getNextChar lexbuf in
 case currChar of
    _ => backtrack lexbuf
 end)
and state_13 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"[" => action_63 lexbuf
 |  #"\"" => action_56 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_48 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_48 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_48 lexbuf
 else case currChar of
    #"'" => state_48 lexbuf
 |  #"_" => state_48 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_65);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => action_43 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_17 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #")" => action_44 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"." => state_44 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_20 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_46);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_39 lexbuf
 else case currChar of
    #"E" => state_32 lexbuf
 |  #"e" => state_32 lexbuf
 |  #"x" => state_36 lexbuf
 |  #"w" => state_40 lexbuf
 |  #"." => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_47);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_38 lexbuf
 else case currChar of
    #"E" => state_32 lexbuf
 |  #"e" => state_32 lexbuf
 |  #"." => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_28 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"1" andalso currChar <= #"9" then  state_30 lexbuf
 else case currChar of
    #"0" => state_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_29 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_50);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_30 lexbuf
 else case currChar of
    #"E" => state_32 lexbuf
 |  #"e" => state_32 lexbuf
 |  #"x" => state_36 lexbuf
 |  #"." => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_50);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_30 lexbuf
 else case currChar of
    #"E" => state_32 lexbuf
 |  #"e" => state_32 lexbuf
 |  #"." => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_31 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_35 lexbuf
 else backtrack lexbuf
 end)
and state_32 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_33 lexbuf
 else case currChar of
    #"~" => state_34 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_33 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_54);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_34 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_33 lexbuf
 else backtrack lexbuf
 end)
and state_35 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_54);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_35 lexbuf
 else case currChar of
    #"E" => state_32 lexbuf
 |  #"e" => state_32 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_36 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_37 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_37 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_37 lexbuf
 else backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_51);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_37 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_37 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_37 lexbuf
 else backtrack lexbuf
 end)
and state_38 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_49);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_38 lexbuf
 else case currChar of
    #"E" => state_32 lexbuf
 |  #"e" => state_32 lexbuf
 |  #"." => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_39 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_48);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_39 lexbuf
 else case currChar of
    #"E" => state_32 lexbuf
 |  #"e" => state_32 lexbuf
 |  #"." => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_40 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_41 lexbuf
 else case currChar of
    #"x" => state_42 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_41 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_52);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_41 lexbuf
 else backtrack lexbuf
 end)
and state_42 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_43 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_43 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_43 lexbuf
 else backtrack lexbuf
 end)
and state_43 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_53);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_43 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_43 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_43 lexbuf
 else backtrack lexbuf
 end)
and state_44 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"." => action_59 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_48 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_45);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_48 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_48 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_48 lexbuf
 else case currChar of
    #"'" => state_48 lexbuf
 |  #"_" => state_48 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_52 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_58 lexbuf
 |  #"&" => state_58 lexbuf
 |  #"%" => state_58 lexbuf
 |  #"$" => state_58 lexbuf
 |  #"#" => state_58 lexbuf
 |  #"+" => state_58 lexbuf
 |  #"*" => state_58 lexbuf
 |  #"-" => state_58 lexbuf
 |  #"/" => state_58 lexbuf
 |  #":" => state_58 lexbuf
 |  #"@" => state_58 lexbuf
 |  #"?" => state_58 lexbuf
 |  #">" => state_58 lexbuf
 |  #"=" => state_58 lexbuf
 |  #"<" => state_58 lexbuf
 |  #"\\" => state_58 lexbuf
 |  #"^" => state_58 lexbuf
 |  #"`" => state_58 lexbuf
 |  #"|" => state_58 lexbuf
 |  #"~" => state_58 lexbuf
 |  #"." => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_53 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_54 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_54 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_54 lexbuf
 else case currChar of
    #"'" => state_54 lexbuf
 |  #"_" => state_54 lexbuf
 |  #"." => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_54 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_54 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_54 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_54 lexbuf
 else case currChar of
    #"'" => state_54 lexbuf
 |  #"_" => state_54 lexbuf
 |  #"." => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_55 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_57 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_57 lexbuf
 else case currChar of
    #"!" => state_56 lexbuf
 |  #"&" => state_56 lexbuf
 |  #"%" => state_56 lexbuf
 |  #"$" => state_56 lexbuf
 |  #"#" => state_56 lexbuf
 |  #"+" => state_56 lexbuf
 |  #"*" => state_56 lexbuf
 |  #"-" => state_56 lexbuf
 |  #"/" => state_56 lexbuf
 |  #":" => state_56 lexbuf
 |  #"@" => state_56 lexbuf
 |  #"?" => state_56 lexbuf
 |  #">" => state_56 lexbuf
 |  #"=" => state_56 lexbuf
 |  #"<" => state_56 lexbuf
 |  #"\\" => state_56 lexbuf
 |  #"^" => state_56 lexbuf
 |  #"`" => state_56 lexbuf
 |  #"|" => state_56 lexbuf
 |  #"~" => state_56 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_56 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_40);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_56 lexbuf
 |  #"&" => state_56 lexbuf
 |  #"%" => state_56 lexbuf
 |  #"$" => state_56 lexbuf
 |  #"#" => state_56 lexbuf
 |  #"+" => state_56 lexbuf
 |  #"*" => state_56 lexbuf
 |  #"-" => state_56 lexbuf
 |  #"/" => state_56 lexbuf
 |  #":" => state_56 lexbuf
 |  #"@" => state_56 lexbuf
 |  #"?" => state_56 lexbuf
 |  #">" => state_56 lexbuf
 |  #"=" => state_56 lexbuf
 |  #"<" => state_56 lexbuf
 |  #"\\" => state_56 lexbuf
 |  #"^" => state_56 lexbuf
 |  #"`" => state_56 lexbuf
 |  #"|" => state_56 lexbuf
 |  #"~" => state_56 lexbuf
 |  #"." => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_57 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_40);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_57 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_57 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_57 lexbuf
 else case currChar of
    #"'" => state_57 lexbuf
 |  #"_" => state_57 lexbuf
 |  #"." => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_58 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_58 lexbuf
 |  #"&" => state_58 lexbuf
 |  #"%" => state_58 lexbuf
 |  #"$" => state_58 lexbuf
 |  #"#" => state_58 lexbuf
 |  #"+" => state_58 lexbuf
 |  #"*" => state_58 lexbuf
 |  #"-" => state_58 lexbuf
 |  #"/" => state_58 lexbuf
 |  #":" => state_58 lexbuf
 |  #"@" => state_58 lexbuf
 |  #"?" => state_58 lexbuf
 |  #">" => state_58 lexbuf
 |  #"=" => state_58 lexbuf
 |  #"<" => state_58 lexbuf
 |  #"\\" => state_58 lexbuf
 |  #"^" => state_58 lexbuf
 |  #"`" => state_58 lexbuf
 |  #"|" => state_58 lexbuf
 |  #"~" => state_58 lexbuf
 |  #"." => state_55 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_60 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_35);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_67 lexbuf
 |  #"&" => state_67 lexbuf
 |  #"%" => state_67 lexbuf
 |  #"$" => state_67 lexbuf
 |  #"#" => state_67 lexbuf
 |  #"+" => state_67 lexbuf
 |  #"*" => state_67 lexbuf
 |  #"-" => state_67 lexbuf
 |  #"/" => state_67 lexbuf
 |  #":" => state_67 lexbuf
 |  #"@" => state_67 lexbuf
 |  #"?" => state_67 lexbuf
 |  #">" => state_67 lexbuf
 |  #"=" => state_67 lexbuf
 |  #"<" => state_67 lexbuf
 |  #"\\" => state_67 lexbuf
 |  #"^" => state_67 lexbuf
 |  #"|" => state_67 lexbuf
 |  #"~" => state_67 lexbuf
 |  #"." => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_61 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_35);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_63 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_63 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_63 lexbuf
 else case currChar of
    #"'" => state_63 lexbuf
 |  #"_" => state_63 lexbuf
 |  #"." => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_63 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_35);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_63 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_63 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_63 lexbuf
 else case currChar of
    #"'" => state_63 lexbuf
 |  #"_" => state_63 lexbuf
 |  #"." => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_64 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_66 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_66 lexbuf
 else case currChar of
    #"!" => state_65 lexbuf
 |  #"&" => state_65 lexbuf
 |  #"%" => state_65 lexbuf
 |  #"$" => state_65 lexbuf
 |  #"#" => state_65 lexbuf
 |  #"+" => state_65 lexbuf
 |  #"*" => state_65 lexbuf
 |  #"-" => state_65 lexbuf
 |  #"/" => state_65 lexbuf
 |  #":" => state_65 lexbuf
 |  #"@" => state_65 lexbuf
 |  #"?" => state_65 lexbuf
 |  #">" => state_65 lexbuf
 |  #"=" => state_65 lexbuf
 |  #"<" => state_65 lexbuf
 |  #"\\" => state_65 lexbuf
 |  #"^" => state_65 lexbuf
 |  #"|" => state_65 lexbuf
 |  #"~" => state_65 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_65 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_36);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_65 lexbuf
 |  #"&" => state_65 lexbuf
 |  #"%" => state_65 lexbuf
 |  #"$" => state_65 lexbuf
 |  #"#" => state_65 lexbuf
 |  #"+" => state_65 lexbuf
 |  #"*" => state_65 lexbuf
 |  #"-" => state_65 lexbuf
 |  #"/" => state_65 lexbuf
 |  #":" => state_65 lexbuf
 |  #"@" => state_65 lexbuf
 |  #"?" => state_65 lexbuf
 |  #">" => state_65 lexbuf
 |  #"=" => state_65 lexbuf
 |  #"<" => state_65 lexbuf
 |  #"\\" => state_65 lexbuf
 |  #"^" => state_65 lexbuf
 |  #"|" => state_65 lexbuf
 |  #"~" => state_65 lexbuf
 |  #"." => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_66 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_36);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_66 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_66 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_66 lexbuf
 else case currChar of
    #"'" => state_66 lexbuf
 |  #"_" => state_66 lexbuf
 |  #"." => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_67 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_35);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_67 lexbuf
 |  #"&" => state_67 lexbuf
 |  #"%" => state_67 lexbuf
 |  #"$" => state_67 lexbuf
 |  #"#" => state_67 lexbuf
 |  #"+" => state_67 lexbuf
 |  #"*" => state_67 lexbuf
 |  #"-" => state_67 lexbuf
 |  #"/" => state_67 lexbuf
 |  #":" => state_67 lexbuf
 |  #"@" => state_67 lexbuf
 |  #"?" => state_67 lexbuf
 |  #">" => state_67 lexbuf
 |  #"=" => state_67 lexbuf
 |  #"<" => state_67 lexbuf
 |  #"\\" => state_67 lexbuf
 |  #"^" => state_67 lexbuf
 |  #"|" => state_67 lexbuf
 |  #"~" => state_67 lexbuf
 |  #"." => state_64 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_71 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => action_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_72 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_34);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #")" => action_32 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_81 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_26);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_84 lexbuf
 else case currChar of
    #"\"" => action_21 lexbuf
 |  #"\\" => action_21 lexbuf
 |  #"b" => action_21 lexbuf
 |  #"a" => action_21 lexbuf
 |  #"f" => action_21 lexbuf
 |  #"n" => action_21 lexbuf
 |  #"r" => action_21 lexbuf
 |  #"t" => action_21 lexbuf
 |  #"v" => action_21 lexbuf
 |  #"\n" => state_82 lexbuf
 |  #"\t" => state_82 lexbuf
 |  #"\r" => state_82 lexbuf
 |  #" " => state_82 lexbuf
 |  #"u" => state_86 lexbuf
 |  #"^" => state_85 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_82 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => state_82 lexbuf
 |  #"\t" => state_82 lexbuf
 |  #"\r" => state_82 lexbuf
 |  #" " => state_82 lexbuf
 |  #"\\" => action_22 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_84 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_92 lexbuf
 else backtrack lexbuf
 end)
and state_85 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"@" andalso currChar <= #"_" then  action_23 lexbuf
 else backtrack lexbuf
 end)
and state_86 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_87 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_87 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_87 lexbuf
 else backtrack lexbuf
 end)
and state_87 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_88 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_88 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_88 lexbuf
 else backtrack lexbuf
 end)
and state_88 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_89 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_89 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_89 lexbuf
 else backtrack lexbuf
 end)
and state_89 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  action_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  action_25 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  action_25 lexbuf
 else backtrack lexbuf
 end)
and state_92 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  action_24 lexbuf
 else backtrack lexbuf
 end)
and state_99 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_19);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\"" => action_16 lexbuf
 |  #"\\" => action_16 lexbuf
 |  #"n" => action_16 lexbuf
 |  #"t" => action_16 lexbuf
 |  #"\n" => state_100 lexbuf
 |  #"\t" => state_100 lexbuf
 |  #"\r" => state_100 lexbuf
 |  #" " => state_100 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_100 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => state_100 lexbuf
 |  #"\t" => state_100 lexbuf
 |  #"\r" => state_100 lexbuf
 |  #" " => state_100 lexbuf
 |  #"\\" => action_17 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_118 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_123 lexbuf
 |  #"&" => state_123 lexbuf
 |  #"%" => state_123 lexbuf
 |  #"$" => state_123 lexbuf
 |  #"#" => state_123 lexbuf
 |  #"+" => state_123 lexbuf
 |  #"*" => state_123 lexbuf
 |  #"-" => state_123 lexbuf
 |  #"/" => state_123 lexbuf
 |  #":" => state_123 lexbuf
 |  #"@" => state_123 lexbuf
 |  #"?" => state_123 lexbuf
 |  #">" => state_123 lexbuf
 |  #"=" => state_123 lexbuf
 |  #"<" => state_123 lexbuf
 |  #"\\" => state_123 lexbuf
 |  #"|" => state_123 lexbuf
 |  #"~" => state_123 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_120 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_122 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_122 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_122 lexbuf
 else case currChar of
    #"'" => state_122 lexbuf
 |  #"_" => state_122 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_122 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_122 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_122 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_122 lexbuf
 else case currChar of
    #"'" => state_122 lexbuf
 |  #"_" => state_122 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_123 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"!" => state_123 lexbuf
 |  #"&" => state_123 lexbuf
 |  #"%" => state_123 lexbuf
 |  #"$" => state_123 lexbuf
 |  #"#" => state_123 lexbuf
 |  #"+" => state_123 lexbuf
 |  #"*" => state_123 lexbuf
 |  #"-" => state_123 lexbuf
 |  #"/" => state_123 lexbuf
 |  #":" => state_123 lexbuf
 |  #"@" => state_123 lexbuf
 |  #"?" => state_123 lexbuf
 |  #">" => state_123 lexbuf
 |  #"=" => state_123 lexbuf
 |  #"<" => state_123 lexbuf
 |  #"\\" => state_123 lexbuf
 |  #"|" => state_123 lexbuf
 |  #"~" => state_123 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_9 lexbuf)

and TokenN lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_8 lexbuf)

and TokenId lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_7 lexbuf)

and TokenIdQ lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_6 lexbuf)

and Comment lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_5 lexbuf)

and String lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_4 lexbuf)

and SkipString lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_3 lexbuf)

and Quotation lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_2 lexbuf)

and SkipQuotation lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_1 lexbuf)

and AntiQuotation lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_71, action_70];
val _ = fn _ => [action_69, action_68, action_67, action_66, action_65, action_64, action_63, action_62, action_61, action_60, action_59, action_58, action_57, action_56, action_55, action_54, action_53, action_52, action_51, action_50, action_49, action_48, action_47, action_46, action_45, action_44, action_43, action_42];
val _ = fn _ => [action_41, action_40, action_39];
val _ = fn _ => [action_38, action_37, action_36, action_35];
val _ = fn _ => [action_34, action_33, action_32, action_31];
val _ = fn _ => [action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20];
val _ = fn _ => [action_19, action_18, action_17, action_16, action_15];
val _ = fn _ => [action_14, action_13, action_12, action_11, action_10, action_9, action_8];
val _ = fn _ => [action_7, action_6, action_5];
val _ = fn _ => [action_4, action_3, action_2, action_1, action_0];

end
