let explode s =
  let rec aux l = function
    | 0 -> l
    | i -> aux ((String.get s i)::l) (i - 1)
  in aux [] (String.length s)

let concat s =
  List.fold_left (fun str elt -> str ^ elt) ""

let rec retaini ops = function
  | 0 -> ops
  | i -> retaini (Ot.RetainOp (ops)) (i - 1)

let rec deletei ops = function
  | 0 -> ops
  | i -> deletei (Ot.DeleteOp (ops)) (i - 1)

let rec addi str ops = function
  | 0 -> ops
  | i -> addi str (Ot.InsertOp ((String.get str (i - 1)), ops)) (i - 1)

let (|>) x f = f x
let flip f a x = f x a

let diffs_of_texts t1 t2 =
  let l1 = String.length t1 in
  let l2 = String.length t2 in
  let compute_ots (endl, endr) (beginl, beginr)  =
    let to_add = String.sub t2 (beginr) (endr - beginr + 1) in
    retaini Ot.EmptyOp (l1 - endl - 1) |>
    flip (addi to_add) (endr - beginr + 1) |>
    flip deletei (endl - beginl + 1) |>
    flip retaini beginl
  in
  let rec iter_reverse = function
    | -1, -1 -> None
    | -1, i -> Some (0, i)
    | i, -1 -> Some (i, 0)
    | left, right ->
      if (String.get t1 left) != (String.get t2 right) then
        Some (left, right)
      else
        iter_reverse (pred left, pred right)
  in let rec iter (left, right) =
    if left >= l1 then
      if right >= l2 then
        None
      else if (String.get t1 left) != (String.get t2 right) then
        Some (left, right)
      else
        iter (succ left, succ right)
    else
    if right >= l2 then
      Some (left, right)
    else
    if (String.get t1 left) != (String.get t2 right) then
      Some (left, right)
    else
      iter (succ left, succ right) in
  let finish = iter_reverse (l1 - 1, l2 - 1) in
  let begining = iter (0, 0) in
  match finish, begining with
  | None, None-> None
  | Some d1, Some d2 -> Some (compute_ots d1 d2)
  | _ -> None
