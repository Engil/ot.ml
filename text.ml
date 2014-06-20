let explode s =
  let rec aux l = function
    | 0 -> l
    | i -> aux ((String.get s i)::l) (i - 1)
  in aux [] (String.length s)

let concat s =
  List.fold_left (fun str elt -> str ^ elt) ""

let diffs_of_texts t1 t2 =
  let l1 = String.length t1 in
  let l2 = String.length t2 in
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
  let finish = iter_reverse (l1, l2) in
  let begining = iter (0, 0) in
  match finish, begining with
  | None, None-> None
  | Some l, Some r -> Some (l, r)
  | _ -> None
