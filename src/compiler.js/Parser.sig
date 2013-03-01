local
in
datatype token =
    ABSTYPE
  | AND
  | ANDALSO
  | ARROW
  | AS
  | BAR
  | CASE
  | CHAR of char
  | COLON
  | COLONGT
  | COMMA
  | DARROW
  | DATATYPE
  | DLBRACE
  | DO
  | DOTDOTDOT
  | DRBRACE
  | ELSE
  | END
  | EOF
  | EQTYPE
  | EQUALS
  | EXCEPTION
  | FN
  | FUN
  | FUNCTOR
  | HANDLE
  | HASH
  | HASHLBRACKET
  | ID of string
  | IF
  | IN
  | INCLUDE
  | INFIX
  | INFIXR
  | LBRACE
  | LBRACKET
  | LET
  | LOCAL
  | LPAREN
  | NEGINT of int
  | NONFIX
  | NZDIGIT of int
  | NZPOSINT2 of int
  | OF
  | OP
  | OPEN
  | ORELSE
  | PRIM_EQTYPE
  | PRIM_REFTYPE
  | PRIM_TYPE
  | PRIM_VAL
  | QUAL_ID of Const.QualifiedIdent
  | QUAL_STAR of Const.QualifiedIdent
  | QUOTEL
  | QUOTEM of string
  | QUOTER of string
  | RAISE
  | RBRACE
  | RBRACKET
  | REAL of real
  | REC
  | RPAREN
  | SEMICOLON
  | SHARING
  | SIG
  | SIGNATURE
  | STAR
  | STRING of string
  | STRUCT
  | STRUCTURE
  | THEN
  | TYPE
  | TYVAR of string
  | UNDERBAR
  | VAL
  | WHERE
  | WHILE
  | WITH
  | WITHTYPE
  | WORD of word
  | ZDIGIT of int
  | ZPOSINT2 of int
end;

val ToplevelPhrase :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Asynt.Dec * bool;
val SigFile :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Asynt.Sig;
val StructFile :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Asynt.Struct;
val TopSpecFile :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Asynt.Sig;
val TopDecFile :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Asynt.Struct;
