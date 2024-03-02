include Jsonoo_intf

module Make (T : Ojs.T) : S with type t = T.t = struct
  module J = Internal.Json (T)

  type t = J.t [@@js]

  exception Decode_error of string

  let decode_error message = raise (Decode_error message)

  let try_parse_opt s =
    try Some (J.parse s) with
    | _ -> None

  let try_parse_exn s =
    try J.parse s with
    | _ -> decode_error ("Failed to parse JSON string \"" ^ s ^ "\"")

  let stringify ?spaces t = J.stringify t ?spaces ()

  module Decode = struct
    type 'a decoder = t -> 'a

    let expected typ t =
      decode_error ("Expected " ^ typ ^ ", got " ^ stringify t)

    let expected_length length array =
      decode_error
        ("Expected array of length "
        ^ string_of_int length
        ^ ", got array of length "
        ^ string_of_int (Array.length array))

    let id t = t

    let null t =
      let js = [%js.of: t] t in
      if Ojs.is_null js then
        Ojs.null
      else
        expected "null" t

    let bool t =
      let js = [%js.of: t] t in
      if Ojs.type_of js = "boolean" then
        [%js.to: bool] js
      else
        expected "boolean" t

    let float t =
      let js = [%js.of: t] t in
      if Ojs.type_of js = "number" then
        [%js.to: float] js
      else
        expected "number" t

    let int t =
      let f = float t in
      if Float.is_finite f && Float.floor f = f then
        int_of_float f
      else
        expected "integer" t

    let string t =
      let js = [%js.of: t] t in
      if Ojs.type_of js = "string" then
        [%js.to: string] js
      else
        expected "string" t

    let char t =
      let s = string t in
      if String.length s = 1 then
        s.[0]
      else
        expected "single-character string" t

    let nullable decode t =
      let js = [%js.of: t] t in
      if Ojs.is_null js then
        None
      else
        Some (decode t)

    let array decode t =
      let js = [%js.of: t] t in
      if Internal.Array.isArray js then
        let array = [%js.to: t array] js in
        let convert i t =
          try decode t with
          | Decode_error message ->
            decode_error (message ^ "\n\tin array at index " ^ string_of_int i)
        in
        Array.mapi convert array
      else
        expected "array" t

    let list decode t = Array.to_list (array decode t)

    let tuple_element decode array i =
      try decode array.(i) with
      | Decode_error message ->
        decode_error (message ^ "\n\tin array at index " ^ string_of_int i)

    let pair decode_a decode_b t =
      let array = array id t in
      if Array.length array = 2 then
        let a = tuple_element decode_a array 0 in
        let b = tuple_element decode_b array 1 in
        (a, b)
      else
        expected_length 2 array

    let tuple2 = pair

    let tuple3 decode_a decode_b decode_c t =
      let array = array id t in
      if Array.length array = 3 then
        let a = tuple_element decode_a array 0 in
        let b = tuple_element decode_b array 1 in
        let c = tuple_element decode_c array 2 in
        (a, b, c)
      else
        expected_length 3 array

    let tuple4 decode_a decode_b decode_c decode_d t =
      let array = array id t in
      if Array.length array = 4 then
        let a = tuple_element decode_a array 0 in
        let b = tuple_element decode_b array 1 in
        let c = tuple_element decode_c array 2 in
        let d = tuple_element decode_d array 3 in
        (a, b, c, d)
      else
        expected_length 4 array

    let object_field (decode : 'a decoder) t key =
      let js = [%js.of: t] t in
      try decode ([%js.to: t] @@ Ojs.get_prop_ascii js key) with
      | Decode_error message ->
        decode_error (message ^ "\n\tin object at field '" ^ key ^ "'")

    let is_object js =
      Ojs.type_of js = "object"
      && (not (Internal.Array.isArray js))
      && not (Ojs.is_null js)

    let dict (decode : 'a decoder) t =
      let js = [%js.of: t] t in
      if is_object js then (
        let keys = Internal.Object.keys js in
        let table = Hashtbl.create (Array.length keys) in
        let set key =
          let value = object_field decode t key in
          Hashtbl.add table key value
        in
        Array.iter set keys;
        table
      ) else
        expected "object" t

    let field key (decode : 'a decoder) t =
      let js = [%js.of: t] t in
      if is_object js then
        if Ojs.get_prop_ascii js key != Ojs.variable "undefined" then
          object_field decode t key
        else
          decode_error ("Expected field '" ^ key ^ "'")
      else
        expected "object" t

    let rec at key_path (decode : 'a decoder) =
      match key_path with
      | [ key ] -> field key decode
      | first :: rest -> field first (at rest decode)
      | [] -> invalid_arg "Expected key_path to contain at least one element"

    let try_optional decode t =
      try Some (decode t) with
      | Decode_error _ -> None

    let try_default value decode t =
      try decode t with
      | Decode_error _ -> value

    let any decoders t =
      let rec inner errors = function
        | [] ->
          let rev_errors = List.rev errors in
          decode_error
            ("Value was not able to be decoded with the given decoders. \
              Errors: "
            ^ String.concat "\n" rev_errors)
        | decode :: rest -> (
          try decode t with
          | Decode_error e -> inner (e :: errors) rest)
      in
      inner [] decoders

    let either a b = any [ a; b ]

    let map f decode t = f (decode t)

    let bind b a t = b (a t) t
  end

  module Encode = struct
    type 'a encoder = 'a -> t

    let id t = t

    let null : t = [%js.to: t] Ojs.null

    let bool b : t = [%js.to: t] @@ [%js.of: bool] b

    let float f : t = [%js.to: t] @@ [%js.of: float] f

    let int i : t = [%js.to: t] @@ [%js.of: int] i

    let string s : t = [%js.to: t] @@ [%js.of: string] s

    let char c : t = string (String.make 1 c)

    let nullable encode = function
      | None -> null
      | Some v -> encode v

    let array encode a : t =
      let encoded : t array = Array.map encode a in
      [%js.to: t] @@ [%js.of: t array] encoded

    let list encode l : t = array encode (Array.of_list l)

    let pair encode_a encode_b (a, b) : t =
      let encoded : t array = [| encode_a a; encode_b b |] in
      [%js.to: t] @@ [%js.of: t array] encoded

    let tuple2 = pair

    let tuple3 encode_a encode_b encode_c (a, b, c) : t =
      let encoded : t array = [| encode_a a; encode_b b; encode_c c |] in
      [%js.to: t] @@ [%js.of: t array] encoded

    let tuple4 encode_a encode_b encode_c encode_d (a, b, c, d) : t =
      let encoded : t array =
        [| encode_a a; encode_b b; encode_c c; encode_d d |]
      in
      [%js.to: t] @@ [%js.of: t array] encoded

    let dict encode table : t =
      let encode_pair ((k : string), v) = (k, [%js.of: t] @@ encode v) in
      table
      |> Hashtbl.to_seq
      |> Array.of_seq
      |> Array.map encode_pair
      |> Ojs.obj
      |> [%js.to: t]

    let object_ (props : (string * t) list) : t =
      let coerce (k, v) = (k, [%js.of: t] v) in
      Array.of_list props |> Array.map coerce |> Ojs.obj |> [%js.to: t]
  end
end

include Make (struct
  type t = Ojs.t [@@js]
end)
