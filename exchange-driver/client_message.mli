(** [Client_message.t] represents the type of message that the client can
    send to the exchange. It should probably not be used directly; instead,
    use the utility functions in exchange_driver.ml. *)

open! Core
open  Types

type t =
  | Hello   of Team_name.t
  | Add     of Order_id.t * Symbol.t * Dir.t * Price.t * Size.t
  | Convert of Order_id.t * Symbol.t * Dir.t * Size.t
  | Cancel  of Order_id.t

(** Converts the message to a json-formatted string to be sent to the
    exchange. *)
val to_string_json : t -> string
