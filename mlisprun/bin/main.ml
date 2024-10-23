open Mlisprun
open Core

(*
let compress_and_write filename data =
  let compressed = Zstd.compress data ~level:1 in
  let length = String.length data in
  Out_channel.with_file filename ~f:(fun oc ->
      Out_channel.output_string oc (string_of_int length);
      Out_channel.newline oc;
      Out_channel.output_string oc compressed)
*)

let read_and_decompress filename =
  In_channel.with_file filename ~f:(fun ic ->
      let length_str = In_channel.input_line_exn ic in
      let length = int_of_string length_str in
      let compressed_data = In_channel.input_all ic in
      Zstd.decompress length compressed_data)

let eval_module prog =
  let env = Parse.resolve prog in
  let x = Eval.eval env in
  match x with
  | None -> Fmt.pr "Error evaluating module"
  | Some x -> Fmt.pr "%a\n" Value.pp x

let files_param =
  let open Command.Param in
  flag "-f" (required string) ~doc:"bytecode file to run"

let main file =
  let bin = read_and_decompress file in
  let prog_sexp = Sexp.of_string bin in
  let prog = Ast.prog_of_sexp prog_sexp in
  eval_module (List.append prog [ `Call { name = "main" } ])

let command =
  Command.basic ~summary:"Run the mlisp bytecode interpreter"
    (let%map_open.Command file_name = files_param in
     fun () -> main file_name)

let _ = Command_unix.run command
