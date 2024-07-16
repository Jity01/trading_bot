open Core
open Import

type t = int ref

let create () = ref 1

let next_id t =
  let this_id = !t in
  incr t;
  Order_id.of_int_exn this_id
;;
