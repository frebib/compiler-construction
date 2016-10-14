open Printf

let usage () = printf "Usage: %s <filename>\n" Sys.argv.(0)

let _ =
  if Array.length Sys.argv < 2
  then (usage (); exit 1)
  else Parse.parse_from Sys.argv.(1)

