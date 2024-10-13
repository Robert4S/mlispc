open Core

type expr =
  | Int of int
  | Eof
  | Atom of string
  | Float of float
  | ConsCell of expr * expr
  | List of expr list
  | Map of expr list
  | String of string
  | Quoted of expr
  | Set of expr list
  | ModAccess of expr * expr
  | TypeCreate of expr * expr list
[@@deriving show { with_path = false }, eq, ord, sexp]

module ExprComparable : Set_intf.Elt with type t = expr = struct
  type t = expr

  let compare = compare_expr
  let t_of_sexp = expr_of_sexp
  let sexp_of_t = sexp_of_expr
end

module ExprSet = Set.Make (ExprComparable)
