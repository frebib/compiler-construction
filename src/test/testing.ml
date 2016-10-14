open Test
open Error
open Printf

let write filename data =
  let out = open_out filename in
  fprintf out "%s" data;
  close_out out;
;;

let parse file_in =
  open_in file_in
  |> Lexing.from_channel
  |> safe Testpar.parse_test Testlex.read file_in
  |> generate_test
;;

let _ = 
  if Array.length Sys.argv < 3
  then (eprintf "Usage: %s <input.test> <output.native>\n" Sys.argv.(0); exit 1)
  else parse Sys.argv.(1)
  |> write Sys.argv.(2)

