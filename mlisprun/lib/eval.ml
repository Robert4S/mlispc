open Core
open Common

let unwrap = function
  | None -> assert false
  | Some x -> x

let skip_until env predicate =
  let rec loop () =
    if predicate Env.(env.code.(env.pc)) then
      let _ = Env.incr env in
      ()
    else
      let _ = Env.incr env in
      loop ()
  in
  loop ()

let eval env =
  let open Option.Let_syntax in
  let ( + ) a b =
    match
      ( Env.which env a |> unwrap |> Register.get,
        Env.which env b |> unwrap |> Register.get )
    with
    | Int i, Int i2 -> Int (i + i2)
    | Float f, Float f2 -> Float (f +. f2)
    | _ -> assert false
  in
  let rec loop () : unit option =
    Env.incr env;
    if env.pc >= Array.length env.code then Some ()
    else
      match env.code.(env.pc) with
      | `Move { from; into } ->
          Env.move env from into |> ignore;
          loop ()
      | `Ret ->
          Env.go_ret env;
          loop ()
      | `Goto label ->
          let%bind () = Env.goto_label env label in
          loop ()
      | `Add { a; b; into } ->
          let%bind () = Env.set env into (a + b) in
          loop ()
      | `Fun { name; length } ->
          Env.add_function env name;
          env.pc <- Int.(env.pc + length);
          loop ()
      | `Data { reg; value } ->
          let%bind () = Env.set env reg (Int value) in
          loop ()
      | `Call { name } ->
          Env.call env name;
          loop ()
      | `JmpEq { r1; r2; label } ->
          let%bind () =
            if Value.equal (Env.get env r1) (Env.get env r2) then
              Env.goto_label env label
            else Some ()
          in
          loop ()
      | `JmpNe { r1; r2; label } ->
          let%bind () =
            if not @@ Value.equal (Env.get env r1) (Env.get env r2) then
              Env.goto_label env label
            else Some ()
          in
          loop ()
      | `Print r ->
          print_endline @@ Value.show @@ Env.get env r;
          loop ()
      | `Mul { r1; r2; into } ->
          let r1 = Env.get env r1 in
          let r2 = Env.get env r2 in
          let res = Value.mul r1 r2 in
          let%bind () = Env.set env into res in
          loop ()
      | `Sub { r1; r2; into } ->
          let r1 = Env.get env r1 in
          let r2 = Env.get env r2 in
          let res = Value.sub r1 r2 in
          let%bind () = Env.set env into res in
          loop ()
      | `Perform { name } ->
          let%bind () = Env.perform env name in
          loop ()
      | `Makefun { into; captures; length } ->
          let%bind () = Env.make_fun env into captures in
          env.pc <- Int.(env.pc + length);
          loop ()
      | `Funcall { reg } ->
          let%bind () = Env.funcall env reg in
          loop ()
  in

  let%map () = loop () in
  Env.ret env |> Register.get
