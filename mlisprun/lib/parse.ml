open Core

let show_token token =
  match token with
  | Parser.RSQUARE -> "]\n"
  | RET -> "RET"
  | MOVE -> "MOV"
  | LSQUARE -> "["
  | INT i -> string_of_int i
  | FLOAT f -> string_of_float f
  | EOF -> "EOF"
  | COMMA -> ", "
  | COLON -> ":"
  | ATOM s -> s
  | LOCAL n -> Format.asprintf "y%d" n
  | GLOBAL n -> Format.asprintf "x%d" n
  | CLOSED n -> Format.asprintf "c%d" n
  | ADD -> "ADD"
  | FUN -> "FUN"
  | CALL -> "CALL"
  | PRINT -> "PRINT"
  | GOTO -> "GOTO"
  | SETLABEL -> "SETLABEL"
  | JNE -> "JNE"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | PERFORM -> "PERFORM"

let show_lexbuf lexbuf =
  Fmt.pr "\n";
  let rec aux () =
    match Lexer.read lexbuf with
    | Parser.EOF -> ()
    | token ->
        Fmt.pr " %s " @@ show_token token;
        aux ()
  in
  aux ();
  Fmt.pr "\n"

let parse (s : string) : Ast.t list =
  let lexbufshow = Lexing.from_string s in
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.prog Lexer.read lexbuf with
    | _ ->
        show_lexbuf lexbufshow;
        show_lexbuf lexbuf;
        assert false
  in
  match ast with
  | None -> []
  | Some o -> o

let resolve ?(env = Env.make ()) (xs : Ast.t list) : Env.t =
  let code = List.to_array @@ List.filter_mapi xs ~f:(Ast.resolve env) in
  let res = Env.with_code env code in
  res
