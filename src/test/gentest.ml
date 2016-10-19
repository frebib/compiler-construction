open Test
open Error
open Printf

let write filename data =
  let out = open_out filename in
  fprintf out "%s" data;
  close_out out;
;;

let parse_from file =
  open_in file
  |> Lexing.from_channel
  |> Error.safe Testpar.parse_test Testlex.read file
  |> name_test file
;;

let _ =
  if Array.length Sys.argv < 3
  then (eprintf "Generates test case programs for the parser program.\nUsage: %s <input.test> <output.native>\n" Sys.argv.(0); exit 1)
  else parse_from Sys.argv.(1)
  |> generate_test
  |> write Sys.argv.(2)

