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

module NormalText = struct
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

module ReverseText = struct
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

module RichText(Text : IterableText) = struct
  type state =
    | Normal
    | Tag of int
    | Quote of int
    | End
  type t = {doc : Text.t; state : state}

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
      if Text.is_at_bound new_doc then
        End
      else if (Text.get_at new_doc) = Text.tag_open then
        match doc.state with
        | Normal -> Tag (Text.get_pos new_doc)
        | Tag _ -> assert false
        | Quote p -> Quote p
        | End -> End
      else if (Text.get_at new_doc) = Text.tag_end then
        match doc.state with
        | Normal -> assert false
        | Tag _ -> Normal
        | Quote p -> Quote p
        | End -> End
      else
        doc.state
    in
    {doc = new_doc; state = new_state}
end
