open Mlisprun.Common

type t = reg_ref [@@deriving show, eq, ord]

let to_bc (reg : t) =
  match reg with
  | `Local i -> Format.asprintf "y%d" i
  | `Global i -> Format.asprintf "x%d" i
  | `Closed i -> Format.asprintf "c%d" i
