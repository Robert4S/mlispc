%{
open Ast
%}

%token <int> INT
%token <string> ATOM
%token <float> FLOAT
%token <string> STRING

%token LPAREN "("
%token RPAREN ")"
%token EOF
%token QUOTE "'"
%token DOT "."
%token LBRAC "{"
%token RBRAC "}"
%token HASH "#"

%start <expr option> prog
%%

prog:
  | EOF { None }
  | es = list(expr); EOF { Some (List es) }
;

expr:
  | module_name = expr; DOT; name = expr {ModAccess (module_name, name)}
  | QUOTE; e = expr {Quoted e}
  | s = STRING {String s}
  | a = ATOM {Atom a}
  | i = INT {Int i}
  | f = FLOAT {Float f}
  | HASH; name = expr; LBRAC; es = list(expr); RBRAC {TypeCreate (name, es)}
  | HASH; LBRAC; es = list(expr); RBRAC {Set es}
  | LBRAC; es = list(expr); RBRAC {Map es}
  | LPAREN; es = list(expr); RPAREN {List es}
;
