(** This module distinguishes between the different modes that the bot can
    run in. *)

open! Core
open! Async

module Test_exchange_type : sig
  type t =
    | Prod_like
    | Slower
    | Empty
end

type t =
  | Testing of Test_exchange_type.t
  | Prod

(** Allows the exchange type to be specified on the command-line. *)
val param : t Command.Param.t

(** Resolves a given exchange type to a hostname and port. This depends on
    values in constants.ml, so ensure that those values are up to date. *)
val hostname_and_port : t -> string * int
