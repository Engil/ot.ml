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
  let diff iter_mod =
    let module T = (val iter_mod : IterableText) in
    let module Diff = RichText(T) in
    let rec iter = function
      | left, right when Diff.is_end left && Diff.is_end right -> None
      | left, right when Diff.is_end left ->
        Some (Diff.get_end left, Diff.get_end right)
      | left, right when Diff.is_end right ->
        Some (Diff.get_end left, Diff.get_end right)
      | left, right ->
        if (Diff.get_at left) != (Diff.get_at right) then
          Some (Diff.get_end left, Diff.get_end right)
      else
        iter (Diff.next left, Diff.next right) in
    iter (Diff.create t1, Diff.create t2) in

  let finish = diff (module ReverseText : IterableText) in
  let begining = diff (module NormalText : IterableText) in

  (* Return a list of operations if a difference is found *)
  match finish, begining with
  | None, None-> None
  | Some (fl, fr), Some (bl, br) -> Some (compute_ots (fl, fr) (bl, br))
  | _ -> None
