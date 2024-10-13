open Core

let ( let* ) x f = Option.bind x ~f

module StringTable_ = Hashtbl.Make (String)

module Stack : sig
  include Stack.S

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end = struct
  include Stack

  type 'a items = 'a list [@@deriving eq, ord]

  let pp pp_value ppf frames =
    Stack.iter frames ~f:(fun frame -> Format.fprintf ppf "[%a]\n" pp_value frame)

  let show pp_value frames = Format.asprintf "%a" (pp pp_value) frames

  let equal eq_value s1 s2 =
    equal_items eq_value (Stack.to_list s1) (Stack.to_list s2)

  let compare compare_value s1 s2 =
    compare_items compare_value (Stack.to_list s1) (Stack.to_list s2)
end

module ST : sig
  include Hashtbl.S with type key = StringTable_.key

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end = struct
  include StringTable_

  type 'a pairs = (string * 'a) list [@@deriving show, eq, ord]

  let pairs table =
    Hashtbl.keys table
    |> List.map ~f:(fun key ->
           let value = Hashtbl.find_exn table key in
           (key, value))

  let pp pp_item ppf table =
    let pairs = pairs table in
    Format.fprintf ppf "{\n";
    Format.fprintf ppf "%a\n" (pp_pairs pp_item) pairs;
    Format.fprintf ppf "}"

  let show pp_item table = Format.asprintf "%a" (pp pp_item) table
  let equal equal_item t1 t2 = equal_pairs equal_item (pairs t1) (pairs t2)
  let compare comp_item t1 t2 = compare_pairs comp_item (pairs t1) (pairs t2)
end

let sequence = Option.all
let sequence_result = Result.all
