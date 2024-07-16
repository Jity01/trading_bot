open! Core
open! Async

type t = { writer : string Pipe.Writer.t }

(* Note: It's very important to include the newline at the end of the
   message, or else the exchange won't process the message. *)
let write_to_exchange t client_message =
  Pipe.write_if_open t.writer (sprintf !"%{Client_message#json}\n" client_message)
;;

let add_order t ~order_id ~symbol ~dir ~price ~size =
  let message = Client_message.Add (order_id, symbol, dir, price, size) in
  write_to_exchange t message
;;

let convert t ~order_id ~symbol ~dir ~size =
  let message = Client_message.Convert (order_id, symbol, dir, size) in
  write_to_exchange t message
;;

let cancel t ~order_id =
  let message = Client_message.Cancel order_id in
  write_to_exchange t message
;;

let connect_and_run exchange_type ~f =
  let host, port = Exchange_type.hostname_and_port exchange_type in
  let where_to_connect =
    Host_and_port.create ~host ~port |> Tcp.Where_to_connect.of_host_and_port
  in
  let%bind _socket, reader, writer = Tcp.connect where_to_connect in
  let reader = Reader.lines reader |> Pipe.map ~f:Exchange_message.of_string_exn in
  let t      = { writer = Writer.pipe writer }                                   in
  let%bind () = write_to_exchange t (Client_message.Hello Constants.team_name) in
  let%bind () = f ~exchange_driver:t ~exchange_messages:reader                 in
  Pipe.close  t.writer;
  Pipe.closed t.writer
;;
