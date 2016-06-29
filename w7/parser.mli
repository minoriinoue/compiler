type token =
  | EOF
  | COMMA
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | TINT
  | TSTRING
  | ASSIGN
  | ARRAY
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | END
  | BREAK
  | NIL
  | FUNCTION
  | VAR
  | PLUS of (Absyn.pos)
  | MINUS of (Absyn.pos)
  | TIMES of (Absyn.pos)
  | DIVIDE of (Absyn.pos)
  | EQ of (Absyn.pos)
  | NEQ of (Absyn.pos)
  | LT of (Absyn.pos)
  | LE of (Absyn.pos)
  | STRING of (string)
  | ID of (string*Absyn.pos)
  | INT of (int)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absyn.exp
