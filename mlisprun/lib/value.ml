open Common

type t = value_t [@@deriving show, eq, ord]

let ( = ) = equal

let mul a b =
  match (a, b) with
  | Int i, Int b -> Int (i * b)
  | _ -> assert false

let sub a b =
  match (a, b) with
  | Int i, Int b -> Int (i - b)
  | _ -> assert false
