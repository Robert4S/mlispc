open Mlispc.Bytecode
open Core

let compress_and_write filename data =
  let compressed = Zstd.compress data ~level:10 in
  let length = String.length data in
  Out_channel.with_file filename ~f:(fun oc ->
      Out_channel.output_string oc (string_of_int length);
      Out_channel.newline oc;
      Out_channel.output_string oc compressed)

let () =
  let prog =
    "\n\
    \    (defun do-something (a) (fun (b) (+ a b)))\n\
    \    (defun main ()\n\
    \      ((do-something 5) 10))\n\
    \    "
  in
  let res = Generate.instructions prog in
  let code = Generate.to_code (List.of_array res) in
  compress_and_write "./test.mlbc" code
