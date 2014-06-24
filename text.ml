let (|>) x f = f x
let flip f a x = f x a

(* Used to handle operations lists *)
let rec retaini ops = function
  | i when i <= 0 -> ops
  | i -> retaini (Ot.RetainOp (ops)) (i - 1)

let rec deletei ops = function
  | i when i <= 0 -> ops
  | i -> deletei (Ot.DeleteOp (ops)) (i - 1)

let rec addi str ops = function
  | i when i <= 0 -> ops
  | i -> addi str (Ot.InsertOp ((String.get str (i - 1)), ops)) (i - 1)

(* Return a diff as a list of operations between two tests *)
let diffs_of_texts t1 t2 =
  let l1 = String.length t1 in
  let l2 = String.length t2 in

  (* Compute the operations list *)
  let compute_ots (endl, endr) (beginl, beginr)  =
    let to_add = String.sub t2 (beginr) (endr - beginr + 1) in
    retaini Ot.EmptyOp (l1 - endl - 1) |>
    flip (addi to_add) (endr - beginr + 1) |>
    flip deletei (endl - beginl + 1) |>
    flip retaini beginl
  in

  (* Comparing from the end of both texts *)
  let rec iter_reverse = function
    | -1, -1 -> None
    | -1,  i -> Some (0, i)
    |  i, -1 -> Some (i, 0)
    | left, right ->
      if (String.get t1 left) != (String.get t2 right) then
        Some (left, right)
      else
        iter_reverse (pred left, pred right)
  in

  (* Comparing from the start *)
  let rec iter (left, right) =
    if left >= (l1 - 1) then
      if right >= (l2 - 1) then
        None
      else if (String.get t1 left) != (String.get t2 right) then
        Some (left, right)
      else
        Some (left, right)
    else
    if right >= (l2 - 1) then
      Some (left, right)
    else
    if (String.get t1 left) != (String.get t2 right) then
      Some (left, right)
    else
      iter (succ left, succ right) in

  let finish = iter_reverse (l1 - 1, l2 - 1) in
  let begining = iter (0, 0) in

  (* Return a list of operations if a difference is found *)
  match finish, begining with
  | None, None-> None
  | Some (fl, fr), Some (bl, br) ->
    if (fl = 0) && (bl = l1 - 1) then
      begin
      Printf.printf "fl %d fr %d bl %d br %d\n" fl fr bl br;
      Some (compute_ots (l1, fr) (bl, br))
        end
    else
      Some (compute_ots (fl, fr) (bl, br))
  | _ -> None
