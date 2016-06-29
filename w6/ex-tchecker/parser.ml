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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"

open Absyn
# 40 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* COMMA *);
  258 (* COLON *);
  259 (* SEMICOLON *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* TINT *);
  263 (* TSTRING *);
  264 (* ASSIGN *);
  265 (* ARRAY *);
  266 (* IF *);
  267 (* THEN *);
  268 (* ELSE *);
  269 (* LET *);
  270 (* IN *);
  271 (* END *);
  272 (* BREAK *);
  273 (* NIL *);
  274 (* FUNCTION *);
  275 (* VAR *);
    0|]

let yytransl_block = [|
  276 (* PLUS *);
  277 (* MINUS *);
  278 (* TIMES *);
  279 (* DIVIDE *);
  280 (* EQ *);
  281 (* NEQ *);
  282 (* LT *);
  283 (* LE *);
  284 (* STRING *);
  285 (* ID *);
  286 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\009\000\
\009\000\010\000\010\000\012\000\012\000\011\000\011\000\013\000\
\013\000\013\000\006\000\006\000\007\000\007\000\008\000\008\000\
\008\000\008\000\003\000\003\000\003\000\005\000\005\000\005\000\
\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\003\000\001\000\001\000\002\000\004\000\
\003\000\003\000\003\000\003\000\006\000\004\000\005\000\001\000\
\001\000\004\000\006\000\001\000\001\000\007\000\009\000\000\000\
\003\000\005\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\000\000\001\000\003\000\000\000\001\000\003\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\006\000\
\000\000\005\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\017\000\007\000\000\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\000\000\000\000\037\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\020\000\021\000\000\000\000\000\015\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000"

let yydgoto = "\002\000\
\011\000\014\000\015\000\013\000\043\000\032\000\033\000\034\000\
\019\000\020\000\021\000\061\000\058\000"

let yysindex = "\004\000\
\003\255\000\000\003\255\003\255\011\255\000\000\003\255\000\000\
\013\255\000\000\000\000\076\255\019\255\068\255\014\255\039\255\
\248\254\020\255\043\255\000\000\000\000\000\000\003\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\255\
\003\255\003\255\003\255\003\255\000\000\003\255\049\255\006\255\
\003\255\021\255\062\255\016\255\000\000\054\255\076\255\000\000\
\095\255\040\255\045\255\003\255\053\255\003\255\000\000\003\255\
\070\255\073\255\000\000\000\000\072\255\076\255\000\000\000\000\
\076\255\045\255\002\255\003\255\080\255\045\255\003\255\076\255\
\040\255\059\255\076\255\000\000\003\255\076\255"

let yyrindex = "\000\000\
\000\000\000\000\079\255\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\085\000\028\000\251\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\081\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\253\254\000\000\000\000\000\000\000\000\
\089\255\100\255\000\000\055\000\000\000\082\000\087\000\000\000\
\114\000\101\255\000\000\000\000\000\000\081\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\094\255\000\000\000\000\
\109\000\000\000\000\000\000\000\105\255\000\000\000\000\097\255\
\101\255\000\000\098\255\000\000\000\000\099\255"

let yygindex = "\000\000\
\000\000\002\000\238\255\000\000\069\000\000\000\000\000\000\000\
\000\000\000\000\000\000\201\255\051\000"

let yytablesize = 385
let yytable = "\036\000\
\041\000\035\000\012\000\070\000\001\000\016\000\003\000\051\000\
\022\000\036\000\069\000\035\000\004\000\052\000\074\000\005\000\
\023\000\048\000\037\000\006\000\039\000\054\000\053\000\007\000\
\042\000\071\000\035\000\003\000\017\000\018\000\008\000\009\000\
\010\000\044\000\045\000\046\000\047\000\026\000\027\000\049\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\040\000\038\000\059\000\060\000\050\000\062\000\009\000\042\000\
\041\000\065\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\055\000\063\000\057\000\072\000\036\000\066\000\
\075\000\024\000\025\000\026\000\027\000\067\000\078\000\068\000\
\073\000\011\000\077\000\035\000\001\000\038\000\012\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\035\000\
\039\000\024\000\056\000\018\000\013\000\025\000\019\000\022\000\
\023\000\014\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\064\000\076\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\000\000\041\000\000\000\041\000\000\000\000\000\
\041\000\000\000\000\000\041\000\041\000\000\000\041\000\041\000\
\000\000\000\000\000\000\000\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\003\000\000\000\003\000\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\003\000\003\000\
\000\000\003\000\003\000\000\000\000\000\000\000\000\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\009\000\
\000\000\009\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\009\000\009\000\000\000\009\000\009\000\000\000\000\000\
\000\000\000\000\009\000\009\000\000\000\000\000\009\000\009\000\
\009\000\009\000\011\000\000\000\011\000\000\000\011\000\012\000\
\000\000\012\000\000\000\012\000\011\000\011\000\000\000\011\000\
\011\000\012\000\012\000\000\000\012\000\012\000\000\000\000\000\
\000\000\011\000\011\000\011\000\011\000\013\000\000\000\013\000\
\000\000\013\000\014\000\000\000\014\000\000\000\014\000\013\000\
\013\000\000\000\013\000\013\000\014\000\000\000\000\000\014\000\
\014\000"

let yycheck = "\005\001\
\000\000\005\001\001\000\002\001\001\000\004\000\004\001\002\001\
\007\000\015\001\066\000\015\001\010\001\008\001\070\000\013\001\
\004\001\036\000\005\001\017\001\029\001\001\001\041\000\021\001\
\023\000\024\001\008\001\000\000\018\001\019\001\028\001\029\001\
\030\001\032\000\033\000\034\000\035\000\022\001\023\001\038\000\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\029\001\011\001\006\001\007\001\004\001\052\000\000\000\054\000\
\014\001\056\000\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\005\001\015\001\029\001\068\000\003\001\002\001\
\071\000\020\001\021\001\022\001\023\001\005\001\077\000\008\001\
\001\001\000\000\024\001\005\001\000\000\005\001\000\000\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\015\001\
\005\001\005\001\012\001\014\001\000\000\005\001\014\001\014\001\
\014\001\000\000\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\054\000\073\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\001\001\255\255\003\001\255\255\
\005\001\255\255\255\255\255\255\255\255\255\255\011\001\012\001\
\255\255\014\001\015\001\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\001\001\
\255\255\003\001\255\255\005\001\255\255\255\255\255\255\255\255\
\255\255\011\001\012\001\255\255\014\001\015\001\255\255\255\255\
\255\255\255\255\020\001\021\001\255\255\255\255\024\001\025\001\
\026\001\027\001\001\001\255\255\003\001\255\255\005\001\001\001\
\255\255\003\001\255\255\005\001\011\001\012\001\255\255\014\001\
\015\001\011\001\012\001\255\255\014\001\015\001\255\255\255\255\
\255\255\024\001\025\001\026\001\027\001\001\001\255\255\003\001\
\255\255\005\001\001\001\255\255\003\001\255\255\005\001\011\001\
\012\001\255\255\014\001\015\001\011\001\255\255\255\255\014\001\
\015\001"

let yynames_const = "\
  EOF\000\
  COMMA\000\
  COLON\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  TINT\000\
  TSTRING\000\
  ASSIGN\000\
  ARRAY\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  IN\000\
  END\000\
  BREAK\000\
  NIL\000\
  FUNCTION\000\
  VAR\000\
  "

let yynames_block = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LE\000\
  STRING\000\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 30 "parser.mly"
 (_1)
# 285 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser.mly"
    (NilExp)
# 291 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : var) in
    Obj.repr(
# 35 "parser.mly"
     (VarExp(_1))
# 298 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : exp list) in
    Obj.repr(
# 37 "parser.mly"
     (SeqExp(_2))
# 305 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 39 "parser.mly"
     (IntExp(_1))
# 312 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
     (StringExp(_1))
# 319 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Absyn.pos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 43 "parser.mly"
     (OpExp(IntExp(0), (MinusOp,_1), _2))
# 327 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string*Absyn.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expseq_comma) in
    Obj.repr(
# 45 "parser.mly"
     (CallExp(_1, _3))
# 335 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Absyn.exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'plusminus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 47 "parser.mly"
     (OpExp(_1, _2, _3))
# 344 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Absyn.exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'multdiv) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 49 "parser.mly"
     (OpExp(_1, _2, _3))
# 353 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Absyn.exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'relop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 51 "parser.mly"
     (OpExp(_1, _2, _3))
# 362 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 53 "parser.mly"
     (AssignExp(_1, _3))
# 370 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Absyn.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Absyn.exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 55 "parser.mly"
     (IfExp(_2, _4, Some(_6)))
# 379 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Absyn.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 57 "parser.mly"
     (IfExp(_2, _4, None))
# 387 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : exp list) in
    Obj.repr(
# 59 "parser.mly"
     (LetExp(_2, SeqExp(_4)))
# 395 "parser.ml"
               : Absyn.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardec) in
    Obj.repr(
# 61 "parser.mly"
             (_1)
# 402 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundec) in
    Obj.repr(
# 62 "parser.mly"
           (_1)
# 409 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string*Absyn.pos) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 66 "parser.mly"
    (VarDec(_2, ref true, None, _4))
# 417 "parser.ml"
               : 'vardec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string*Absyn.pos) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 68 "parser.mly"
    (VarDec(_2, ref true, Some(_4), _6))
# 426 "parser.ml"
               : 'vardec))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
          (IntTy)
# 432 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
            (StringTy)
# 438 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string*Absyn.pos) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'tyfields) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Absyn.pos) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 75 "parser.mly"
    (FunctionDec(_2,_4,None, _7))
# 448 "parser.ml"
               : 'fundec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string*Absyn.pos) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'tyfields) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : Absyn.pos) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 77 "parser.mly"
    (FunctionDec(_2,_4,Some(_7), _9))
# 459 "parser.ml"
               : 'fundec))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
           ([])
# 465 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string*Absyn.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 81 "parser.mly"
    ([(_1, ref true, _3)])
# 473 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string*Absyn.pos) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'tyfields) in
    Obj.repr(
# 83 "parser.mly"
    ((_1, ref true, _3)::_5)
# 482 "parser.ml"
               : 'tyfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.pos) in
    Obj.repr(
# 85 "parser.mly"
                 ((PlusOp,_1))
# 489 "parser.ml"
               : 'plusminus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.pos) in
    Obj.repr(
# 86 "parser.mly"
          ((MinusOp,_1))
# 496 "parser.ml"
               : 'plusminus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.pos) in
    Obj.repr(
# 88 "parser.mly"
                ((TimesOp,_1))
# 503 "parser.ml"
               : 'multdiv))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.pos) in
    Obj.repr(
# 89 "parser.mly"
           ((DivideOp,_1))
# 510 "parser.ml"
               : 'multdiv))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.pos) in
    Obj.repr(
# 92 "parser.mly"
       ((EqOp,_1))
# 517 "parser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.pos) in
    Obj.repr(
# 93 "parser.mly"
        ((NeqOp,_1))
# 524 "parser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.pos) in
    Obj.repr(
# 94 "parser.mly"
       ((LtOp,_1))
# 531 "parser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.pos) in
    Obj.repr(
# 95 "parser.mly"
       ((LeOp,_1))
# 538 "parser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
     ([])
# 544 "parser.ml"
               : exp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 100 "parser.mly"
     ([(_1)])
# 551 "parser.ml"
               : exp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Absyn.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : exp list) in
    Obj.repr(
# 102 "parser.mly"
     ((_1)::_3)
# 559 "parser.ml"
               : exp list))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
     ([])
# 565 "parser.ml"
               : 'expseq_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Absyn.exp) in
    Obj.repr(
# 107 "parser.mly"
     ([_1])
# 572 "parser.ml"
               : 'expseq_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Absyn.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expseq_comma) in
    Obj.repr(
# 109 "parser.mly"
     (_1::_3)
# 580 "parser.ml"
               : 'expseq_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string*Absyn.pos) in
    Obj.repr(
# 112 "parser.mly"
     (_1)
# 587 "parser.ml"
               : var))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Absyn.exp)
