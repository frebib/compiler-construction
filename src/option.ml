let get = function
  | Some a -> a
  | None   -> failwith "Option.get"

let present = function
  | Some _ -> true
  | None   -> false
;;
let absent a = not (present a)

let map fn = function
  | Some a -> fn a
  | None   -> None
;;
let map_def fn def = function
  | Some a -> fn a
  | None   -> def
;;

let string_of_opt fn = function
  | Some a -> "Some " ^ fn a
  | None   -> "None"
;;
