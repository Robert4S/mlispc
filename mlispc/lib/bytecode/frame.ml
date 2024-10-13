open Core
open Common

type 'value t = {
  symbols : 'value ST.t;
  mutable local_count : int;
  functions : Bcast.func ST.t;
}
[@@deriving eq, ord, show]

let create
    ?(symbols = ST.create ())
    ?(local_count = 0)
    ?(functions = ST.create ())
    () =
  { symbols; local_count; functions }

let incr frame =
  let curr = frame.local_count in
  frame.local_count <- curr + 1;
  `Local curr

let find_var { symbols; _ } name = Hashtbl.find symbols name
let find_fun { functions; _ } name = Hashtbl.find functions name
let set_var { symbols; _ } name value = Hashtbl.set symbols ~key:name ~data:value
