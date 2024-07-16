open Core

(** The [Stringable] module type provides string serialization, as well as
    data structures like maps, sets, hashtables, and hashsets. *)
module type Stringable = sig
  type t [@@deriving sexp, compare, bin_io]

  val to_string : t -> string
  val of_string_exn : string -> t

  include Comparable.S_binable with type t := t
  include Hashable.S           with type t := t
end

(** The [Intable] module type provides all the same things as [Stringable],
    as well as conversion from and to ints. *)
module type Intable = sig
  type t

  val to_int : t -> int
  val of_int_exn : int -> t

  include Stringable with type t := t
end

module Symbol : sig
  include Stringable

  val bond  : t
  val vale  : t
  val valbz : t
  val xlf   : t
  val gs    : t
  val ms    : t
  val wfc   : t
end

module Team_name : Stringable
module Price     : Intable
module Size      : Intable
module Position  : Intable
module Order_id  : Intable

module Dir : sig
  type t =
    | Buy
    | Sell
  [@@deriving sexp, compare, bin_io]

  include Stringable with type t := t
end

(** The [Dirpair.t] type is a container that holds the same type of value for
    both the buy and sell directions. It's useful for guaranteeing that we
    have values for both sides. *)
module Dirpair : sig
  type 'a t =
    { buy  : 'a
    ; sell : 'a
    }
  [@@deriving sexp, bin_io]

  val create : buy:'a -> sell:'a -> 'a t
end
