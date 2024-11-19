open Core
open Common

type op =
  [ `Fun of string * int
  | `Call of string
  | `Data of Value.t * [ `Float of float | `Int of int ]
  | `Move of Value.t * Value.t
  | `Ret
  | `Perform of string
  | `Makefun of Value.t * int list * int
  | `Funcall of Value.t ]
[@@deriving show]

type 'value t = { stack : 'value Frame.t Stack.t; functions : Bcast.func ST.t }
[@@deriving eq, ord, show]

let push env = Stack.push env.stack (Frame.create ~functions:env.functions ())
let pop env = Stack.pop_exn env.stack |> ignore

let rec find_fun ({ functions; _ } as env) name =
  let open Option in
  match Hashtbl.find functions name with
  | Some f -> Some f
  | None -> find_var env name >>| fun a -> Bcast.Closure a

and add_fun env (func : Bcast.func) =
  (let* name =
     match func with
     | Userdefined { name; _ } -> Some name
     | Intern name -> Some name
     | Closure _ -> None
   in
   Some (Hashtbl.set env.functions ~key:name ~data:func))
  |> ignore

and handle env node =
  match node with
  | Bcast.Defun func ->
      add_fun env func;
      None
  | other -> Some other

and set_functions env (ast : Bcast.t list) = List.filter_map ast ~f:(handle env)

and find_var (env : 'value t) name : 'value option =
  let* frame = Stack.top env.stack in
  Frame.find_var frame name

let set_var (env : 'value t) (value : 'value) name : (unit, string) result =
  match Stack.top env.stack with
  | Some frame ->
      Frame.set_var frame name value;
      Ok ()
  | None -> Error "empty stack"

let incr env =
  Option.(Stack.top env.stack >>| fun frame -> Frame.incr frame)
  |> Result.of_option
       ~error:"tried to increment y register counter on an empty stack frame"

let rec handle_call env funct args =
  let open Result.Let_syntax in
  let%bind prefix, final_call =
    match funct with
    | Bcast.Atom name ->
        Ok
          (match find_fun env name with
          | Some (Bcast.Intern name) -> ([||], [| `Perform name |])
          | Some (Userdefined { name; _ }) -> ([||], [| `Call name |])
          | Some (Closure reg) -> ([||], [| `Funcall reg |])
          | None -> assert false)
    | other ->
        let%bind funreg = incr env in
        let%map ops = eval_into env funreg other in
        (ops, [| `Funcall funreg |])
  in
  let%map pairs =
    List.(
      args >>| fun expr ->
      let open Result.Let_syntax in
      let%bind loc = incr env in
      let%map value = eval_into env loc expr in
      (loc, value))
    |> sequence_result
  in
  let moves = List.mapi pairs ~f:(fun idx (loc, _) -> `Move (loc, `Global idx)) in
  let arg_evals = List.bind pairs ~f:(fun (_, instr) -> List.of_array instr) in
  let setup = List.append arg_evals moves in
  Array.append prefix @@ Array.append (List.to_array setup) final_call

and eval_into : Value.t t -> Value.t -> Bcast.t -> (op array, string) result =
 fun env register ast ->
  match ast with
  | Bcast.Int i -> Ok [| `Data (register, `Int i) |]
  | Float f -> Ok [| `Data (register, `Float f) |]
  | Atom name ->
      let open Result.Let_syntax in
      let%map var =
        find_var env name
        |> Result.of_option ~error:(Format.sprintf "unbound symbol %s" name)
      in
      [| `Move (var, register) |]
  | Call { funct; args } ->
      let open Result.Let_syntax in
      let%map call = handle_call env funct args in
      let res = Array.append call [| `Move (`Global 0, register) |] in
      res
  | Fun { args; body } -> eval_fun env register args body
  | If { cond; then_; else_ } -> eval_if env register cond then_ else_
  | Defun _ -> assert false

and eval_if env register cond then_ else_ =
  let open Result.Let_syntax in
  let%bind cond_addr = incr env in
  let%bind cond = eval_into env cond_addr cond in
  let%bind then_ = eval_into env register then_ in
  let%bind else_ = eval_into env register else_ in
  let then_ = Array.to_list then_ in
  let else_ = Array.to_list else_ in
  assert false

and free_vars env body args =
  let vars = bound body in
  let vars = subtract_vars vars args in
  subtract_vars vars (builtin_names env)

and eval_fun :
    Value.t t -> Value.t -> string list -> Bcast.t -> (op array, string) result =
 fun env reg args body ->
  let open Result.Let_syntax in
  let vars = free_vars env body args in
  let%bind regs =
    List.map vars ~f:(fun var ->
        find_var env var
        |> Result.of_option ~error:(Format.sprintf "Unbound: %s" var))
    |> sequence_result
  in
  let capture_nums = get_captures regs in
  let%bind regs =
    List.map regs ~f:(fun reg ->
        match reg with
        | `Local x -> Ok (`Closed x)
        | `Global x -> Ok (`Global x)
        | `Closed _ -> Error "double closure capture not implemented")
    |> sequence_result
  in
  push env;
  let pairs = List.zip_exn vars regs in
  List.iter pairs ~f:(fun (name, reg) -> set_var env reg name |> unwrap_result);
  let%bind arg_sets = set_args env args in
  let%bind evaluate = eval_into env (`Global 0) body in
  let before_ret = Array.append (List.to_array arg_sets) evaluate in
  let result = Array.append before_ret [| `Ret |] in
  let length = Array.length result in
  let result = Array.append [| `Makefun (reg, capture_nums, length) |] result in
  pop env;
  return result

and builtin_names env =
  env.functions |> Hashtbl.keys
  |> List.filter_map ~f:(fun key ->
         match Hashtbl.find env.functions key with
         | Some (Bcast.Intern _) -> Some key
         | _ -> None)

and eval_function_def : Value.t t -> Bcast.userdef -> (op array, string) result =
 fun env { args; body; name } ->
  push env;
  let open Result.Let_syntax in
  let%bind arg_sets = set_args env args in
  let%bind evaluate = eval_into env (`Global 0) body in
  let before_ret = Array.append (List.to_array arg_sets) evaluate in
  let result = Array.append before_ret [| `Ret |] in
  let length = Array.length result in
  let result = Array.append [| `Fun (name, length) |] result in
  pop env;
  return result

and eval : Value.t t -> Bcast.userdef list -> (op array, string) result =
 fun env functions ->
  let open Result.Let_syntax in
  let%map subarrays =
    List.map functions ~f:(eval_function_def env) |> sequence_result
  in
  let combined = List.bind ~f:List.of_array subarrays in
  List.to_array combined

and bound : Bcast.t -> string list =
 fun expr ->
  match expr with
  | Atom a -> [ a ]
  | Fun { body; _ } -> bound body
  | Call { funct; args } -> bound funct @ List.(args >>= bound)
  | _ -> []

and subtract_vars from sub =
  List.filter from ~f:(fun var -> not @@ List.mem sub var ~equal:String.equal)

and unwrap_result = function
  | Ok a -> a
  | _ -> assert false

and set_args env args =
  let open Result.Let_syntax in
  List.mapi args ~f:(fun idx arg ->
      let%bind loc = incr env in
      let%map () = set_var env loc arg in
      `Move (`Global idx, loc))
  |> sequence_result

and get_captures regs =
  List.filter_map regs ~f:(fun reg ->
      match reg with
      | `Local x -> Some x
      | _ -> None)

let with_functions =
  let interns = Mlisprun.Env.interns |> Hashtbl.keys in
  let table = ST.create () in
  List.iter interns ~f:(fun name ->
      Hashtbl.set table ~key:name ~data:(Bcast.Intern name));
  table

let create ?(stack = Stack.create ()) ?(functions = with_functions) () =
  { stack; functions }

let optimise _env arr =
  Array.filter_map arr ~f:(fun instr ->
      match instr with
      | `Move (from, into) when Value.equal from into -> None
      | other -> Some other)
