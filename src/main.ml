open Parse
open Printf

let _ =
  if Array.length Sys.argv < 2
  then (printf "Parses languages and prints the tree\nUsage: %s <filename>\n" Sys.argv.(0); exit 1)
  else parse_print_from Sys.argv.(1)

