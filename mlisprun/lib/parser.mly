%token <int> INT
%token <string> ATOM
%token <float> FLOAT
%token <int> LOCAL
%token <int> GLOBAL
%token <int> CLOSED

%token EOF
%token MOVE
%token COMMA
%token LSQUARE
%token RSQUARE
%token COLON
%token RET
%token ADD
%token FUN
%token CALL
%token GOTO
%token PRINT
%token SETLABEL
%token JNE
%token MUL
%token SUB
%token PERFORM

%start <Ast.t list option> prog

%%

prog:
  | EOF {None}
  | es = list(form); EOF {Some (es)}
;


form:
  | o = op {o}
;

op:
  | MOVE; from = reg; COMMA; into = reg {`Move Common.{from; into}}
  | RET {`Ret}
  | reg = reg; COLON; value = INT {`Data Common.{reg; value}}
  | ADD; a = reg; COMMA; b = reg; COMMA; into = reg {`Add Common.{a; b; into}}
  | FUN; name = ATOM; COMMA; length = INT; {`Fun Common.{name; length}}
  | CALL; name = ATOM {`Call Common.{name}}
  | GOTO; label = ATOM {`Goto label}
  | PRINT; reg = reg {`Print reg}
  | SETLABEL; name = ATOM {`SetLabel name}
  | JNE; r1 = reg; COMMA; r2 = reg; COMMA; label = ATOM {`JmpNe Common.{r1; r2; label}}
  | MUL; r1 = reg; COMMA; r2 = reg; COMMA; into = reg {`Mul Common.{r1; r2; into}}
  | SUB; r1 = reg; COMMA; r2 = reg; COMMA; into = reg {`Sub Common.{r1; r2; into}}
  | PERFORM; name = ATOM {`Perform Common.{name}}
;

reg:
  | i = GLOBAL {`Global i}
  | i = LOCAL {`Local i}
  | i = CLOSED {`Closed i}
