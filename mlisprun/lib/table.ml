open Core
open Common

let generation = ref 1

let gen () =
  let curr = !generation in
  generation := curr + 1;
  curr

type frame = {
  registers : Value.t Register.t array;
  closure_env : closure option;
  generation : int;
  labels : int StringTable.t;
  ret : int;
}
[@@deriving show]

type t = frame Stack.t

let pp ppf frames =
  Stack.iter frames ~f:(fun frame -> Format.fprintf ppf "[%a]\n" pp_frame frame)

let show frames = Format.asprintf "%a" pp frames

module Option = struct
  include Option

  let or_else v ~f =
    match v with
    | Some x -> Some x
    | None -> f ()
end

let make_frame ?(closure_env = None) ~length ~labels ~ret () =
  {
    registers = Array.init length ~f:(fun _ -> Register.make (Int 0 : Value.t));
    generation = gen ();
    closure_env;
    labels;
    ret;
  }

let make () =
  let s = Stack.create () in
  Stack.push s @@ make_frame ~length:20 ~labels:(StringTable.create ()) ~ret:(-1) ();
  s

let get stack regname =
  match stack with
  | [] -> None
  | x :: _ -> Some (Register.get x.registers.(regname))

let set frames regname data =
  Register.set (Stack.top_exn frames).registers.(regname) data

let push_frame ?(closure_env = None) ~labels ~length ~ret x =
  Stack.push x (make_frame ~labels ~length ~ret ~closure_env ())

let top frames = Stack.top frames

let top_exn frames =
  match top frames with
  | Some x -> x
  | None -> assert false

let pop_frame table =
  let open Option in
  Stack.pop table >>= fun frame ->
  generation := !generation - 1;
  Some frame

let closed frame ~reg =
  let open Option.Let_syntax in
  let%map closures = frame.closure_env in
  closures.env.(reg)
