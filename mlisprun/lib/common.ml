open Core

type reg_ref = [ `Local of int | `Global of int | `Closed of int ]
[@@deriving show, eq, sexp, ord]

type mov = { from : reg_ref; into : reg_ref } [@@deriving show, eq, sexp]
type data = { reg : reg_ref; value : int } [@@deriving show, eq, sexp]
type call = { name : string } [@@deriving show, eq, sexp]
type load = { into : reg_ref; alloc : int } [@@deriving show, eq, sexp]
type set = { alloc : int; reg : reg_ref } [@@deriving show, eq, sexp]
type add = { a : reg_ref; b : reg_ref; into : reg_ref } [@@deriving show, eq, sexp]
type func = { name : string; length : int } [@@deriving show, eq, sexp]
type funcall = { reg : reg_ref } [@@deriving show, eq, sexp]

type makefun = { into : reg_ref; captures : int list; length : int }
[@@deriving show, eq, sexp]

type 'a pairs = (string * 'a) list [@@deriving show, eq, sexp]

type jmp_eq = { r1 : reg_ref; r2 : reg_ref; label : string }
[@@deriving show, eq, sexp]

type jmp_ne = { r1 : reg_ref; r2 : reg_ref; label : string }
[@@deriving show, eq, sexp]

type bin_op = { r1 : reg_ref; r2 : reg_ref; into : reg_ref }
[@@deriving show, eq, sexp]

module ST = Hashtbl.Make (String)

module StringTable = struct
  include ST

  let pp pp_val ppf tbl =
    let pairs =
      Hashtbl.keys tbl |> List.map ~f:(fun key -> (key, Hashtbl.find_exn tbl key))
    in
    Format.fprintf ppf "{";
    Format.fprintf ppf "%a" (pp_pairs pp_val) pairs;
    Format.fprintf ppf "}"

  let show pp_val tbl = Format.asprintf "%a" (pp pp_val) tbl
end

module Register : sig
  type 'a t [@@deriving show, eq, ord]

  val mov : into:'a t -> from:'a t -> 'a
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
  val make : 'a -> 'a t
end = struct
  type 'a t = { mutable inner : 'a } [@@deriving eq, ord]

  (** Moves a value into a register and returns the previous value **)
  let mov ~(into : 'a t) ~(from : 'a t) =
    let curr = into.inner in
    into.inner <- from.inner;
    curr

  let get x = x.inner
  let set r v = r.inner <- v
  let make i = { inner = i }
  let pp pp_val ppf reg = Format.fprintf ppf "Register ( %a )" pp_val reg.inner
  let show pp_val reg = Format.asprintf "%a" (pp pp_val) reg
end

type closure = {
  env : value_t Register.t Array.t;
      [@printer
        fun ppf env ->
          Array.iter env ~f:(fun reg ->
              Format.fprintf ppf "%a" (Register.pp pp_value_t) reg)]
  addr : int;
}
[@@deriving show, eq, ord]

and value_t = Int of int | Float of float | Function of int | Closure of closure
[@@deriving show, eq, ord]
