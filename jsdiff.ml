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

let onload _ =
  let d = Html.document in
  let editor =
    Js.Opt.get (d##getElementById (Js.string "editor"))
      (fun () -> assert false) in
  Printf.printf "B: %d E: %d\n" (length_start_tag editor) (length_end_tag
  editor); Js._true

let _ = Html.window##onload <- Html.handler onload
(*
(*
let diff_html left right =

  let retrieve_nodes stack =
    let stack = Stack.create () in
    let max = node##childNodes##length in
    for i = max - 1 downto 0 do
      Stack.push (Js.Opt.get (node##childNodesitem (i)))
        (fun () -> assert false)
    done in

  let rec inner (stack_left, pos_left) (stack_right, pos_right) =
    let nodel = Stack.pop stack_left in
    let noder = Stack.pop stack_right in

    if nodel##nodeType = noder##nodeType then
      if nodel##nodeType = Dom.TEXT then
        (* compare text *)
      else
        (* test node equality *)
    else
      (* end the diff *)
*)
*)
