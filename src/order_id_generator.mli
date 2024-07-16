(** This module provides a way to generate unique, sequential order ids,
    starting with 1. You don't have to use it, but it'll probably make coming
    up with order ids a bit easier. *)

open Import

type t

(** Initiate [t]. *)
val create  : unit -> t

(** Generate the next unused order ID. *)
val next_id : t    -> Order_id.t
