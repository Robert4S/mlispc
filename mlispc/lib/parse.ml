open Ast

let parse (s : string) : expr list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with 
  | Some (List xs) -> xs
  | _ -> []


