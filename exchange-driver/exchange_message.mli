(** [Exchange_message.t] represents the type of message that the exchange can
    send to the client. *)

open Core
open Types

module Book : sig
  type t =
    { symbol : Symbol.t
    ; book   : (Price.t * Size.t) list Dirpair.t
    }
  [@@deriving sexp_of]
end

module Trade : sig
  type t =
    { symbol : Symbol.t
    ; price  : Price.t
    ; size   : Size.t
    }
  [@@deriving sexp_of]
end

module Reject : sig
  type t =
    { order_id : Order_id.t
    ; error    : Error.t
    }
  [@@deriving sexp_of]
end

module Fill : sig
  type t =
    { order_id : Order_id.t
    ; symbol   : Symbol.t
    ; dir      : Dir.t
    ; price    : Price.t
    ; size     : Size.t
    }
  [@@deriving sexp_of]
end

type t =
  | Hello  of (Symbol.t * Position.t) list
  | Open   of Symbol.Set.t
  | Close  of Symbol.Set.t
  | Error  of Error.t
  | Book   of Book.t
  | Trade  of Trade.t
  | Ack    of Order_id.t
  | Reject of Reject.t
  | Fill   of Fill.t
  | Out    of Order_id.t
[@@deriving sexp_of]

(** Raises if the message is malformed. (The exchange should never send
    malformed messages; that would be a bug.) *)
val of_string_exn : string -> t
