open Common
type t = value_t [@@deriving show, eq, ord]

val (=) : t -> t -> bool
val mul : t -> t -> t
val sub : t -> t -> t
