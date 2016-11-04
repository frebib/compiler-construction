open Eval
open Types

let constant = function
  | Empty
  | Ref _
  | Const _
  | Boolean _ -> true
  | _         -> false

let rec optimise store env e = e |> function
  | Empty
  | Const   _
  | Boolean _
  | Asg     _
  | Deref   _
  | Application _ 
  | Readint
  | Printint _
  | Ref     _          -> e

  | UnaryOp (op, e)    -> UnaryOp (op, e) 
  | BinaryOp (o, a, b) -> let a' = optimise store env a in
                          let b' = optimise store env b in
                          (match a', b' with
    | Const a, Const b -> (match o with
      | Plus    -> Const (a + b)
      | Minus   -> Const (a - b)
      | Times   -> Const (a * b)
      | Divide  -> Const (a / b)
      | Modulus -> Const (a mod b)
      | Noteq   -> Boolean (a != b)
      | Equal   -> Boolean (a = b)
      | Lth     -> Boolean (a < b)
      | Gth     -> Boolean (a > b)
      | Leq     -> Boolean (a <= b)
      | Geq     -> Boolean (a >= b)
      | _       -> BinaryOp (o, a', b'))
    | Boolean a, Boolean b -> (match o with
      | And     -> Boolean (a && b)
      | Or      -> Boolean (a || b)
      | Noteq   -> Boolean (a != b)
      | Equal   -> Boolean (a = b)
      | _       -> BinaryOp (o, a', b'))
    | _ -> BinaryOp (o, a', b'))

  | If (g, a, b)       -> let g' = optimise store env g in
                          if constant g' then
                            (* Assume the guard always evaluates to a boolean *)
                            let g'' = (match g' with Boolean b -> b | _ -> failwith "optimise: if") in
                            if g'' then a else b
                          else
                            If (g', optimise store env a, optimise store env b)

  | While (g, b)       -> While (optimise store env g, optimise store env b)

  | Seq l              -> List.map (optimise store env) l |> List.filter (fun e -> e != Empty) |> seq_of_list

  | Identifier s       -> (try Hashtbl.find env s with _ -> e)
  | Function (a, b)    -> Function (a, optimise store env b)
  | BoundFunction (a, b, ht) -> BoundFunction (a, optimise store env b, ht)

  | Let (v, e, i)      -> let value = optimise store env e in
                          if constant value then
                            (Hashtbl.add env v value;
                            let tree = optimise store env i in
                            Hashtbl.remove env v;
                            tree)
                          else
                            Let (v, value, (optimise store env i))

  | New (v, e, i)      -> New (v, optimise store env e, optimise store env i)


let optimise_prog = optimise (Hashtbl.create 8) (Hashtbl.create 8)
