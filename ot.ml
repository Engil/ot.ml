(* Code generated with Coq from the following repository *)
(* https://github.com/Operational-Transformation/ot.v *)

type 'a t =
  | EmptyOp
  | RetainOp of int * 'a t
  | InsertOp of 'a * 'a t
  | DeleteOp of 'a t

let fmap = (fun f a -> match a with Some e -> Some (f e) | None -> None)

let rec list_op_rect f f0 f1 f2 = function
  | EmptyOp -> f
  | RetainOp (0, l0) -> f0 l0 (list_op_rect f f0 f1 f2 l0)
  | RetainOp (i, l0) -> f0 (RetainOp (pred i, l0)) (list_op_rect f f0 f1 f2 (RetainOp (pred i, l0)))
  | InsertOp (a, l0) -> f1 a l0 (list_op_rect f f0 f1 f2 l0)
  | DeleteOp l0 -> f2 l0 (list_op_rect f f0 f1 f2 l0)

let rec list_op_rec f f0 f1 f2 = function
  | EmptyOp -> f
  | RetainOp (0, l0) -> f0 l0 (list_op_rec f f0 f1 f2 l0)
  | RetainOp (i, l0) -> f0 (RetainOp (pred i, l0)) (list_op_rec f f0 f1 f2 (RetainOp (pred i, l0)))
  | InsertOp (a, l0) -> f1 a l0 (list_op_rec f f0 f1 f2 l0)
  | DeleteOp l0 -> f2 l0 (list_op_rec f f0 f1 f2 l0)

let rec addDeleteOp o = match o with
  | InsertOp (i, o') -> InsertOp (i, (addDeleteOp o'))
  | _ -> DeleteOp o

let rec start_length = function
  | EmptyOp -> 0
  | RetainOp (0, o') -> succ (start_length o')
  | RetainOp (i, o') -> succ (start_length (RetainOp (pred i, o')))
  | InsertOp (a, o') -> start_length o'
  | DeleteOp o' -> succ (start_length o')

let rec end_length = function
  | EmptyOp -> 0
  | RetainOp (0, o') -> succ (end_length o')
  | RetainOp (i, o') -> succ (end_length (RetainOp (pred i, o')))
  | InsertOp (a, o') -> succ (end_length o')
  | DeleteOp o' -> end_length o'

let rec apply o l =
  match o with
  | EmptyOp ->
    (match l with
     | "" -> Some ""
     | _ -> None)
  | RetainOp (i, o) ->
    let o' = if i = 0 then o else RetainOp (pred i, o) in
    (match l with
     | "" -> None
     | s ->
       let x = Char.escaped (String.get s 0) in
       let xs = String.sub s 1 ((String.length s) - 1) in
       fmap (fun xs' -> x ^ xs') (apply o' xs))
  | InsertOp (x, o') -> fmap (fun l' -> (Char.escaped x) ^ l') (apply o' l)
  | DeleteOp o' ->
    (match l with
     | "" -> None
     | s ->
       let xs = String.sub s 1 ((String.length s) - 1) in
       apply o' xs)

let rec normalize = function
  | EmptyOp -> EmptyOp
  | RetainOp (i, o) ->
    let o' = if i = 0 then o else RetainOp (pred i, o) in RetainOp (pred i, normalize o')
  | InsertOp (c, o') -> InsertOp (c, (normalize o'))
  | DeleteOp o' -> addDeleteOp (normalize o')

let rec compose a =
  let rec compose' b =
    match a with
    | EmptyOp ->
      (match b with
       | EmptyOp -> Some EmptyOp
       | InsertOp (c, b') -> fmap (fun x -> InsertOp (c, x)) (compose' b')
       | _ -> None)
    | RetainOp (i, a) ->
      let a' = if i = 0 then a else RetainOp(pred i, a) in
      (match b with
       | EmptyOp -> None
       | RetainOp (i, b) -> let b' = if i = 0 then b else RetainOp(pred i, b) in fmap (fun x -> RetainOp (i, x)) (compose a' b')
       | InsertOp (c, b') -> fmap (fun x -> InsertOp (c, x)) (compose' b')
       | DeleteOp b' -> fmap addDeleteOp (compose a' b'))
    | InsertOp (c, a') ->
      (match b with
       | EmptyOp -> None
       | RetainOp (i, b) -> let b' = if i = 0 then b else RetainOp(pred i, b) in fmap (fun x -> InsertOp (c, x)) (compose a' b')
       | InsertOp (c0, b') -> fmap (fun x -> InsertOp (c0, x)) (compose' b')
       | DeleteOp b' -> compose a' b')
    | DeleteOp a' -> fmap addDeleteOp (compose a' b)
  in compose'

let option_join = function
  | Some m' -> m'
  | None -> None

let pair_map f g p = (f (fst p)),(g (snd p))

let option_pair_map f g mp = fmap (pair_map f g) mp

let rec transform a =
  let rec transform' b =
    match a with
    | EmptyOp ->
      (match b with
       | EmptyOp -> Some (EmptyOp,EmptyOp)
       | InsertOp (c, b') ->
         option_pair_map (fun x -> RetainOp (c, x)) (fun x -> InsertOp (c, x))
           (transform' b')
       | _ -> None)
    | RetainOp (i, a) ->
      let a' = if i = 0 then a else RetainOp(pred i, a) in
      (match b with
       | EmptyOp -> None
       | RetainOp (i, c) ->
         let c' = if i = 0 then c else RetainOp(pred i, c) in
         option_pair_map (fun x -> RetainOp (i, x)) (fun x -> RetainOp (i, x))
           (transform a' c')
       | InsertOp (c, b') ->
         option_pair_map (fun x -> RetainOp (1, x)) (fun x -> InsertOp (c, x))
           (transform' b')
       | DeleteOp b' ->
         option_pair_map (fun x -> x) addDeleteOp (transform a' b'))
    | InsertOp (c, a') ->
      option_pair_map (fun x -> InsertOp (c, x)) (fun x -> RetainOp (1 ,x))
        (transform a' b)
    | DeleteOp a' ->
      (match b with
       | EmptyOp -> None
       | RetainOp (i, c) ->
         let c' = if i = 0 then c else RetainOp(pred i, c) in
         option_pair_map addDeleteOp (fun x -> x) (transform a' c')
       | InsertOp (c, b') ->
         option_pair_map (fun x -> RetainOp (1, x)) (fun x -> InsertOp (c, x))
           (transform' b')
       | DeleteOp b' -> transform a' b')
  in transform'
