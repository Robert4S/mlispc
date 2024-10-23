open Core
open Common

exception Idk of string * string

let interns =
  let tbl = StringTable.create () in
  let funs =
    [
      ( "+",
        ( 2,
          fun vals ->
            match (vals.(0), vals.(1)) with
            | Int i, Int i2 -> Int (i + i2)
            | Float f, Float f2 -> Float (f +. f2)
            | other1, other2 -> raise (Idk (Value.show other1, Value.show other2)) )
      );
    ]
  in
  List.iter funs ~f:(fun (key, data) -> Hashtbl.set tbl ~key ~data);
  tbl

type t = {
  mutable pc : int;
  code : Op.t array;
  stack : Table.t;
  registers : Value.t Register.t array;
  functions : int StringTable.t;
  frame_gen : unit; [@printer fun ppf _ -> Format.fprintf ppf "%d" !Table.generation]
  labels : int StringTable.t;
  interns : (int * (Value.t Array.t -> Value.t)) StringTable.t;
}
[@@deriving show]

let make
    ?(pc = -1)
    ?(stack = Table.make ())
    ?(code = [||])
    ?(registers = Array.init 20 ~f:(fun _ -> Register.make (Int 0)))
    ?(labels = StringTable.create ())
    ?(functions = StringTable.create ())
    ?(interns = interns)
    () =
  { pc; stack; registers; code; functions; frame_gen = (); labels; interns }

let incr e = e.pc <- e.pc + 1
let with_code env code = { env with code }
let debug e = Format.printf "\n%s\n" @@ show e

let go_ret e =
  let frame = Table.top_exn e.stack in
  Table.pop_frame e.stack |> ignore;
  e.pc <- frame.ret

let which env reg =
  let open Option in
  match reg with
  | `Closed x -> Table.top env.stack >>= Table.closed ~reg:x
  | `Local x -> Table.top env.stack >>= fun r -> Some r.registers.(x)
  | `Global x -> Some env.registers.(x)

let set e reg v =
  match which e reg with
  | Some r -> Some (Register.set r v)
  | None -> None

let move e from into =
  let open Option.Let_syntax in
  let which = which e in
  let%bind from = which from in
  let%bind into = which into in
  return (Register.mov ~into ~from)

let ret e = e.registers.(0)
let set_label e ?(pos = e.pc) (s : string) = Hashtbl.set e.labels ~key:s ~data:pos

let goto_label e label =
  let open Option.Let_syntax in
  let%bind frame = Table.top e.stack in
  let%map addr = Hashtbl.find frame.labels label in
  e.pc <- addr

let call e name =
  Table.push_frame e.stack ~labels:e.labels ~length:10 ~ret:e.pc;
  let f = Hashtbl.find_exn e.functions name in
  e.pc <- f

let funcall e reg =
  let open Option.Let_syntax in
  let%bind reg = which e reg in
  match Register.get reg with
  | Closure c ->
      Table.push_frame e.stack ~labels:e.labels ~length:10 ~ret:e.pc
        ~closure_env:(Some c);
      Some (e.pc <- c.addr)
  | _ -> None

let add_function e name = Hashtbl.set e.functions ~key:name ~data:e.pc

type exn += OutOfBounds of { env : t; reg : reg_ref }

let exn_printer = function
  | OutOfBounds { env; reg } ->
      Some
        (Format.asprintf "Out of bounds: Env=%s,\n reg=%s" (show env)
           (show_reg_ref reg))
  | _ -> None

let get e r =
  match which e r with
  | Some r -> Register.get r
  | None -> raise @@ OutOfBounds { env = e; reg = r }

let perform e name =
  let open Option.Let_syntax in
  let%bind arity, func = Hashtbl.find e.interns name in
  let args = Array.slice e.registers 0 arity |> Array.map ~f:Register.get in
  let result = func args in
  set e (`Global 0) result

let sequence xs =
  let rec do_seq xs acc =
    match xs with
    | Some x :: xs -> do_seq xs (x :: acc)
    | None :: _ -> None
    | [] -> Some acc
  in
  do_seq xs []

let make_fun e into captures =
  let open Option.Let_syntax in
  let%bind n_env =
    captures |> List.map ~f:(fun i -> which e (`Local i)) |> sequence
  in
  let env = List.to_array n_env in
  let clj = { env; addr = e.pc } in
  let%map into = which e into in
  Register.set into (Closure clj)
