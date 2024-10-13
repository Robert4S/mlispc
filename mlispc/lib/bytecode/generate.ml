open Core
open Common

let instructions text =
  let unwrap a =
    match a with
    | Some v -> v
    | None -> assert false
  in
  let unwrap_res a =
    match a with
    | Ok v -> v
    | Error e -> failwith e
  in
  let env = Env.create () in
  let parsed = Parse.parse text in
  let bca = List.map ~f:Bcast.of_expr parsed |> sequence |> unwrap in
  let _ = Env.set_functions env bca in
  let funcs =
    env.functions |> Hashtbl.keys
    |> List.map ~f:(Hashtbl.find_exn env.functions)
    |> List.filter_map ~f:(function
         | Bcast.Intern _ -> None
         | Closure _ -> None
         | Userdefined u -> Some u)
  in
  Env.eval env funcs |> unwrap_res |> Env.optimise env

let to_run_inst (instr : Env.op) : Mlisprun.Ast.t =
  match instr with
  | `Fun (name, length) -> `Fun { name; length }
  | `Data (reg, `Int value) -> `Data { reg; value }
  | `Data _ -> assert false
  | `Move (from, into) -> `Move { from; into }
  | `Ret -> `Ret
  | `Call name -> `Call { name }
  | `Perform name -> `Perform { name }
  | `Makefun (into, captures, length) -> `Makefun { into; captures; length }
  | `Funcall reg -> `Funcall { reg }

type instructions = Env.op list [@@deriving show]

let to_code instrs =
  Mlisprun.Ast.sexp_of_prog (List.map instrs ~f:to_run_inst) |> Sexp.to_string
