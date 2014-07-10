module type IterableText = sig
  type t
  val tag_open : char
  val tag_end : char
  val next : t -> t
  val create : string -> t
  val get_bound : t -> int
  val get_pos : t -> int
  val get_at : t -> char
  val is_at_bound : t -> bool
end

module IterNormal = struct
  type t = {document : string; bound : int; pos : int}
  let tag_open = '<'
  let tag_end = '>'
  let next doc = {document = doc.document; bound = doc.bound; pos = succ doc.pos;}
  let create text = {document = text; bound = (String.length text); pos = 0}
  let get_bound doc = doc.bound
  let get_pos doc = doc.pos
  let get_at doc = String.get doc.document doc.pos
  let is_at_bound doc = doc.bound = doc.pos
end

module IterReverse = struct
  let tag_open = '>'
  let tag_end = '<'
  type t = {document : string; bound : int; pos : int}
  let next doc = {document = doc.document; bound = doc.bound; pos = pred doc.pos;}
  let create text = {document = text; bound = (-1); pos = (String.length text - 1)}
  let get_bound doc = doc.bound
  let get_pos doc = doc.pos
  let get_at doc = String.get doc.document doc.pos
  let is_at_bound doc = doc.bound = doc.pos
end

module Diff(Text : IterableText) = struct
  type state =
    | Normal
    | Tag of int
    | Quote of (char * int)
    | End of state
  type t = {doc : Text.t; state : state;}

  let is_quote c = c = '\'' || c = '"'

  let create s =
    let new_doc = (Text.create s) in
    let new_state =
      let cur = Text.get_at new_doc in
      if cur = Text.tag_open then
        Tag (Text.get_pos new_doc)
      else
        Normal
    in
    { doc = new_doc; state = new_state }

  let next doc =
    let new_doc = Text.next doc.doc in
    let new_state =
      let current = Text.get_at new_doc in
      if Text.is_at_bound new_doc then
        End doc.state
      else if current = Text.tag_open then
        match doc.state with
        | Normal -> Tag (Text.get_pos new_doc)
        | Tag _ -> assert false
        | Quote (c, p) -> Quote (c, p)
        | End prev -> End prev
      else if current = Text.tag_end then
        match doc.state with
        | Normal -> assert false
        | Tag _ -> Normal
        | Quote (c, p) -> Quote (c, p)
        | End prev -> End prev
      else if is_quote current then
        match doc.state with
        | Normal -> Normal
        | Tag p -> Quote (current, p)
        | Quote (c, p) -> if c = current then Tag p else Quote (c, p)
        | End prev -> End prev
      else
        doc.state
    in
    {doc = new_doc; state = new_state}

  let get_state doc = doc.state

  let get_at doc = Text.get_at doc.doc

  let is_end doc = match doc.state with End _ -> true | _ -> false

  let get_end doc doc2 =
    let get_position d =
      match d.state with
      | End prev ->
        begin
          match prev with
          | Normal -> Text.get_pos d.doc
          | End _ -> assert false
          | Quote (c, p) -> p
          | Tag p -> p
        end
      | Normal -> Text.get_pos d.doc
      | Quote (c, p) -> p
      | Tag p -> p in
    let pos_left = get_position doc in
    let pos_right = get_position doc2 in
    match doc.state, doc2.state with
      | Normal, Tag p
      | End _, Tag p
      | Normal, Tag p
      | End _, Tag p -> pos_right, pos_right
      | _ -> pos_left, pos_right

end
