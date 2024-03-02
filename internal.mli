module Array : sig
  val isArray : 'a -> bool [@@js.global]
end [@js.scope "Array"]

module Json (T : Ojs.T) : sig
  type t = T.t

  val t_of_js : Ojs.t -> t

  val t_to_js : t -> Ojs.t

  val parse : string -> t [@@js.global]

  val stringify :
       t
    -> ?replacer:(Ojs.t[@js.default Ojs.variable "undefined"])
    -> ?spaces:int
    -> unit
    -> string
  [@@js.global]
end
[@@js.scope "JSON"]

module Object : sig
  val keys : 'a -> string array [@@js.global]
end
[@@js.scope "Object"]
