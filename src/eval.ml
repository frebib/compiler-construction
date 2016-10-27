open Error
open Types
open Print
open Printf

let rec map_bool fn e = fn (get_bool e)
and get_bool = function
  | Boolean b -> b
  | Const i   -> (i > 0)
  | e -> raise (eval_error ("Not a boolean type: " ^ string_of_exp e))
and map_int fn e  = fn (get_int e)
and map_int_inc e = Const (map_int (fun i -> i + 1) e)
and map_int_dec e = Const (map_int (fun i -> i - 1) e)
and get_int = function
  | Const i   -> i
  | e -> raise (eval_error ("Not an integer type: " ^ string_of_exp e))

and exp_compare a b = match a, b with
  | Const i, Const j     -> compare i j
  | Boolean i, Boolean j -> compare i j
  | _, _ -> compare a b (* This will probably fail but yolo *)
;;

(* Environment manipulation *)
let rec find_var store = function
  | Ref s | Identifier s -> (try
                      Hashtbl.find store s
                    with
                      | Not_found -> raise (eval_error (sprintf "Variable '%s' is not defined" s))
                      | e -> raise e)
  | e -> raise (eval_error ("Not a variable. Can't lookup: " ^ string_of_exp e))

and put_var store e v = match e with
  | Ref s | Identifier s -> Hashtbl.add store s v
  | e -> raise (eval_error ("Not a variable. Can't store: " ^ string_of_exp e))

and map_var store fn = function
  (* Fetch, map and update the value. Return the Identifier *)
  | e -> find_var store e |> fn |> put_var store e; e

(* Binding arguments to functions *)
let rec bind_args fn args = match fn, args with
  | Function (l, b), _ -> bind_args (BoundFunction (l, b, (Hashtbl.create (List.length l)))) args
  | BoundFunction ([], _, _),     []    -> fn (* All arguments applied *)
  | BoundFunction ([], b, ht),    a::tl -> let expected = (Hashtbl.length ht) in
                                           let applied  = expected + (List.length args) in
                                           raise (eval_error ("Function applied to too many arguments. " ^
                                           sprintf "Function expects %d but %d were applied" expected applied))

  | BoundFunction (p::ps, b, ht), a::tl -> Hashtbl.add ht p a;
                                           bind_args (BoundFunction (ps, b, ht)) tl

  | e, _ -> raise (eval_error ("Not a function type: " ^ (string_of_exp e)))


let rec eval_exp store env e = printf " > %s\n" (string_of_exp e); match e with
  | Asg (e1, e2)    -> let rhs = eval_exp store env e2 in
                       let lhs = eval_exp store env e1 in
                       put_var store lhs rhs; Empty

  | If (e, tr, fa)  -> if eval_exp store env e |> get_bool
                       then eval_exp store env tr
                       else eval_exp store env fa

  | While (e, body) -> let ret = ref Empty in
                       while get_bool (eval_exp store env e) do
                         ret := eval_exp store env body
                       done;
                       !ret

  | UnaryOp (op, e) -> (match op with
                        | PreInc  -> map_var env map_int_inc e |> eval_exp store env
                        | PreDec  -> map_var env map_int_dec e |> eval_exp store env
                        | PostInc -> eval_exp store env e |> map_var env map_int_inc
                        | PostDec -> eval_exp store env e |> map_var env map_int_dec
                        | Not     -> Boolean (eval_exp store env e |> map_bool (not))
                        | _ -> raise (eval_error ("Not a unary operator: " ^ string_of_op op)))

  | BinaryOp (op, e1, e2) -> let v1 = eval_exp store env e1 in
                             let v2 = eval_exp store env e2 in
                             (match op with
                              | Plus    -> Const   ((get_int v1)  +  (get_int v2))
                              | Minus   -> Const   ((get_int v1)  -  (get_int v2))
                              | Times   -> Const   ((get_int v1)  *  (get_int v2))
                              | Divide  -> Const   ((get_int v1)  /  (get_int v2))
                              | Modulus -> Const   ((get_int v1)  mod (get_int v2))
                              | And     -> Boolean ((get_bool v1) && (get_bool v2))
                              | Or      -> Boolean ((get_bool v1) || (get_bool v2))
                              | Noteq   -> Boolean ((get_bool v1) != (get_bool v2))
                              | Equal   -> Boolean ((exp_compare v1 v2) == 0)
                              | Lth     -> Boolean ((exp_compare v1 v2) < 0)
                              | Gth     -> Boolean ((exp_compare v1 v2) > 0)
                              | Leq     -> Boolean ((exp_compare v1 v2) <= 0)
                              | Geq     -> Boolean ((exp_compare v1 v2) >= 0)
                              | _ -> raise (eval_error ("Not a binary operator" ^ string_of_op op)))

  | Application (id, args) -> let fn = eval_exp store env id in
                              let bound = bind_args fn (List.map (eval_exp store env) args) in
                              (match bound with
      | BoundFunction ([], body, ht) -> let subenv = Hashtbl.copy env in
                                        Hashtbl.iter (Hashtbl.add subenv) ht;
                                        eval_exp store subenv body
      | BoundFunction _              -> bound (* Return the partially applied function *)
      | _ -> failwith "This will never happen") (* Now just watch me be proved wrong... *)

  | Let (v, e, i)  -> let value = eval_exp store env e in
                      Hashtbl.add env v value;
                      let ret = eval_exp store env i in
                      Hashtbl.remove env v; ret

  | New (v, e, i)  -> let value = eval_exp store env e in
                      Hashtbl.add store v value;
                      Hashtbl.add env v (Ref v);
                      printf "ENV:\n";
                      Hashtbl.iter (fun k v -> printf "%s: %s\n" k (string_of_exp v)) env;
                      printf "\nSTORE:\n";
                      Hashtbl.iter (fun k v -> printf "%s: %s\n" k (string_of_exp v)) store;
                      printf "\n\n";
                      eval_exp store env i

  | Printint e   -> printf "%d\n" (get_int (eval_exp store env e)); Empty
  | Readint      -> Const (read_line () |> int_of_string)

  | Seq (hd::tl) -> eval_exp store env hd |> ignore; eval_exp store env (seq_of_list tl)
  | Deref e      -> (eval_exp store env e |> function
    | Ref s -> find_var store (Identifier s)
    | e     -> raise (eval_error ("Can't dereference a non-reference type: " ^ string_of_exp e)))
  
  | Identifier s -> let v = find_var env (Identifier s) in printf " ~ %s: %s" s (string_of_exp v); v
  | Ref s        -> printf "%s\n" (string_of_exp (Ref s)); Ref s
  | e            -> e

let eval exp = eval_exp (Hashtbl.create 8) (Hashtbl.create 8) exp
let eval_all l = eval (Seq l)

