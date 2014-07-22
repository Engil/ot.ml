module Html = Dom_html

let length_start_tag node =
  if node##nodeType != Dom.ELEMENT then
    assert false
  else
  if node##childNodes##length != 0 then
    node##outerHTML##indexOf(node##innerHTML)
  else
    node##outerHTML##length

let length_end_tag node =
  if node##nodeType != Dom.ELEMENT then
    assert false
  else
  if node##childNodes##length != 0 then
    let index_inner = node##outerHTML##indexOf(node##innerHTML) in
    node##outerHTML##length - (index_inner + node##innerHTML##length)
  else
    -1

let append_ops op ops =
  let rec aux = function
    | Ot.EmptyOp -> op
    | Ot.InsertOp (c, os) -> Ot.InsertOp (c, (aux os))
    | Ot.DeleteOp os -> Ot.DeleteOp (aux os)
    | Ot.RetainOp (i, os) -> Ot.RetainOp (i, (aux os))
  in aux ops



let diff_html left right =

  let get_node node index =
    Js.Opt.get (((node##childNodes)##item index)) (fun _ -> None) in

  let ops_from_nonde_right n = [] in
  let ops_from_nonde_left n = [] in
  let flatten_ops ops = ops in


  let diff_elt n1 n2 = false in

  let rec inner n1 p1 n2 p2 =

    match n1##nodeType, n2##noteType with
    | Dom.TEXT, Dom.TEXT ->
        let t1 = n1##textContent in
        let t2 = n2##textContent in
        if t1 != t2 then
         (* [Ot.DeleteOp (String.length t1) *) Ot.DeleteOp (Ot.InsertOp (t2, Ot.EmptyOp))
        else
          Ot.RetainOp (String.length t1, (Ot.EmptyOp))

    | Dom.ELEMENT, Dom.ELEMENT ->
        if diff_elt n1 n2 then
          Ot.DeleteOp ( Ot.InsertOp ("c", Ot.EmptyOp))(* Ot.DeleteOp (String.length (n1##outerHTML)); Ot.InsertOp (n2##outerHTML) *)
        else
          let l1 = n1##childNodes##length in
          let l2 = n2##childNodes##length in
          let list_ops = ref Ot.EmptyOp in
          let min, max = if l1 > l2 then l2, l1 else l1, l2 in
          for i = 0 to min do
            match get_node n1 i, get_node n2 i with
            | Some node, None -> list_ops := Ot.EmptyOp (* list_ops := !list_ops :: ops_from_none_right node *)
            | None, Some node -> list_ops := Ot.EmptyOp (* list_ops := !list_ops :: ops_from_none_left node *)
            | None, None -> list_ops := Ot.EmptyOp (* list_ops := flatten_ops list_ops *)
            | Some n1, Some n2 -> list_ops := Ot.EmptyOp (* list_ops := (!list_ops :: inner n1 p1 n2 p2) *)
          done;
          !list_ops

    (* | a, b when a##nodeType != b##nodeType -> Ot.EmptyOp *)
    | _, _ -> (* wat case *) assert false in
  inner left 0 right 0

let onload _ =
  let d = Html.document in
  let editor =
    Js.Opt.get (d##getElementById (Js.string "editor"))
      (fun () -> assert false) in
  let editor2 =
    Js.Opt.get (d##getElementById (Js.string "editor"))
      (fun () -> assert false) in
  Printf.printf "B: %d E: %d\n" (length_start_tag editor) (length_end_tag
  editor);
  diff_html editor editor2;
  Js._true

let _ = Html.window##onload <- Html.handler onload
