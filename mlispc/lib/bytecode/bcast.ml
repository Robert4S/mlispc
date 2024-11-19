open Core
open Common

type userdef = { name : string; args : string list; body : t } [@@deriving show, eq]
and func = Userdefined of userdef | Intern of string | Closure of Value.t

and t =
  | Defun of func
  | Call of { funct : t; args : t list }
  | Int of int
  | Float of float
  | Atom of string
  | Fun of { args : string list; body : t }
  | If of { cond : t; then_ : t; else_ : t }
[@@deriving show, eq, ord]

exception Unsupported of string

let rec of_expr (expr : Ast.expr) : t option =
  let open Option in
  match expr with
  | List [ Atom "defun"; Atom name; List args; body ] ->
      let* body = of_expr body in
      let args =
        List.map args ~f:(function
          | Atom s -> s
          | _ -> assert false)
      in
      return @@ Defun (Userdefined { body; name; args })
  | List [ Atom "fun"; List args; body ] ->
      let* body = of_expr body in
      let args =
        List.map args ~f:(function
          | Atom s -> s
          | _ -> assert false)
      in
      return @@ Fun { args; body }
  | List [ Atom "if"; cond; then_; else_ ] ->
      let* cond = of_expr cond in
      let* then_ = of_expr then_ in
      let* else_ = of_expr else_ in
      return @@ If { cond; then_; else_ }
  | List (funct :: args) ->
      let* args = List.map args ~f:of_expr |> sequence in
      let* funct = of_expr funct in
      return @@ Call { funct; args }
  | Int i -> return @@ Int i
  | Float f -> return @@ Float f
  | Atom name -> return @@ Atom name
  | other -> raise (Unsupported (Ast.show_expr other))
