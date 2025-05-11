module Widget = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let widget_type = Jv.get Jv.global "__CM__widgetType"

  let make to_dom =
    let w = Jv.new' widget_type [||] in
    Jv.set w "toDOM" (Jv.callback ~arity:1 (fun _view -> to_dom ()));
    w
end

type t = Jv.t

include (Jv.Id : Jv.CONV with type t := t)

let decoration = Jv.get Jv.global "__CM__decoration"

let widget ?block ?side w =
  let block =
    match block with None -> [] | Some b -> [ ("block", Jv.of_bool b) ]
  in
  let side =
    match side with None -> [] | Some s -> [ ("side", Jv.of_int s) ]
  in
  let spec =
    Jv.obj (Array.of_list (("widget", Widget.to_jv w) :: (block @ side)))
  in
  Jv.call decoration "widget" [| spec |] |> of_jv

module Range = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
end

let range ~from ~to_ t =
  Jv.call t "range" [| Jv.of_int from; Jv.of_int to_ |] |> Range.of_jv

module Range_set = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let range_set = Jv.get Jv.global "__CM__rangeSet"

  let of' ranges =
    Jv.call range_set "of" [| Jv.of_array Range.to_jv ranges |] |> of_jv
end
