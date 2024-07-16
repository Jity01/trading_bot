open! Core
open! Async
open  Types

type t

(** [connect_and_run] establishes a TCP socket connection to the exchange and
    initiates a HELLO message. The caller is given access (via the [f]
    callback) to a pipe of messages read from the exchange along with an
    [Exchange_driver.t] to be used for sending messages to the exchange. *)
val connect_and_run
  :  Exchange_type.t
  -> f:
       (exchange_driver:t
        -> exchange_messages:Exchange_message.t Pipe.Reader.t
        -> unit Deferred.t)
  -> unit Deferred.t

(** [add_order], [convert], and [cancel] are utilities for sending messages to the
    exchange. The returned deferreds become determined when the message is written to the
    exchange (but not necessarily after the exchange processes the message). *)
val add_order
  :  t
  -> order_id:Order_id.t
  -> symbol:Symbol.t
  -> dir:Dir.t
  -> price:Price.t
  -> size:Size.t
  -> unit Deferred.t

val convert
  :  t
  -> order_id:Order_id.t
  -> symbol:Symbol.t
  -> dir:Dir.t
  -> size:Size.t
  -> unit Deferred.t

val cancel : t -> order_id:Order_id.t -> unit Deferred.t
