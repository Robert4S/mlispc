{
  open Parser
}

let digit = ['0'-'9']
let int = '-'? digit digit*
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t' ',' '\n' '\r']+
let id = [^ '.' '(' ')' '\t' '\n' '\r' ' ' ';' '"' '{' '}' '#']*

rule read =
  parse
  | white {read lexbuf}
  | int {INT (int_of_string (Lexing.lexeme lexbuf))}
  | "." {DOT}
  | "#" {HASH}
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "'" {QUOTE}
  | "{" {LBRAC}
  | "}" {RBRAC}
  | "." {DOT}
  | id {ATOM (Lexing.lexeme lexbuf)}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "\""      { read_string (Buffer.create 17) lexbuf }
  | eof {EOF}

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
