{
  open Parser
}

let digit = ['0'-'9']
let num = digit*
let white = [' ' '\t'  '\n' '\r']+
let id = [^ '.' ':' '(' ')' '[' ']' '\t' '\n' '\r' ' ' ',' ';' '"' '{' '}' '#']*
let global = 'x' num
let local = 'y' num
let closed = 'c' num

rule read =
  parse
  | "," {COMMA}
  | white {read lexbuf}
  | global {GLOBAL (int_of_string (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 1)))}
  | closed {CLOSED (int_of_string (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 1)))}
  | local {LOCAL (int_of_string (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 1)))}
  | num {INT (int_of_string (Lexing.lexeme lexbuf))}
  | "MOV" {MOVE}
  | "RET" {RET}
  | "ADD" {ADD}
  | "FUN" {FUN}
  | "CALL" {CALL}
  | "GOTO" {GOTO}
  | "PRINT" {PRINT}
  | "SETLABEL" {SETLABEL}
  | "JNE" {JNE}
  | "MUL" {MUL}
  | "SUB" {SUB}
  | "PERFORM" {PERFORM}
  | ":" {COLON}
  | "[" {LSQUARE}
  | "]" {RSQUARE}
  | id {ATOM (Lexing.lexeme lexbuf)}
  | eof {EOF}

and read_string buf =
  parse
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
