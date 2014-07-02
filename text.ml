open Diff

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
  (* We are using the positions where differences starts *)
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
    let module Diff = Diff(T) in
    Diff.(
      let rec iter = function
        | left, right when is_end left && is_end right -> None
        | left, right when is_end left -> Some (get_end left, get_end right)
        | left, right when is_end right -> Some (get_end left, get_end right)
        | left, right -> if (get_at left) != (get_at right) then
            Some (get_end left, get_end right)
          else
            iter (next left, next right) in
      iter (create t1, create t2)) in

  let finish = diff (module IterReverse : IterableText) in
  let begining = diff (module IterNormal : IterableText) in

  (* Return a list of operations if a difference is found *)
  (* There is an ugly hack to avoid problem when texts are something like "lol" and "lolol" *)
  (* The problem is that when parsing the text it will match in the two orders *)
  (* So we must detect that case and force to match the first part only *)
  (* Is obviously a FIXME *)
  match finish, begining with
  | None, None-> None
  | Some (fl, fr), Some (bl, br) ->
    let br, fr = if fr < br then bl, (l2 - 1) else br, fr in
    let bl, fl = if fl < bl then bl, (l1 - 1) else bl, fl in
    Some (compute_ots (fl, fr) (bl, br))
  | _ -> None
