open Core
open Types

module Book = struct
  type t =
    { symbol : Symbol.t
    ; book   : (Price.t * Size.t) list Dirpair.t
    }
  [@@deriving sexp_of]
end

module Trade = struct
  type t =
    { symbol : Symbol.t
    ; price  : Price.t
    ; size   : Size.t
    }
  [@@deriving sexp_of]
end

module Reject = struct
  type t =
    { order_id : Order_id.t
    ; error    : Error.t
    }
  [@@deriving sexp_of]
end

module Fill = struct
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

let of_string_exn message =
  let open Yojson.Basic.Util in
  let json                = Yojson.Basic.from_string message                   in
  let message_type        = json |> member "type" |> to_string                 in
  let single_symbol json  = json |> member "symbol" |> to_string |> Symbol.of_string_exn in
  let symbol_list json ~f = json |> member "symbols" |> to_list |> List.map ~f in
  let symbol_set json =
    symbol_list json ~f:(fun s -> to_string s |> Symbol.of_string_exn)
    |> Symbol.Set.of_list
  in
  let order_id json = json |> member "order_id" |> to_int    |> Order_id.of_int_exn in
  let error    json = json |> member "error"    |> to_string |> Error.of_string     in
  let price    json = json |> member "price"    |> to_int    |> Price.of_int_exn    in
  let size     json = json |> member "size"     |> to_int    |> Size.of_int_exn     in
  match message_type with
  | "hello" ->
    let symbols =
      symbol_list json ~f:(fun json ->
        let position = json |> member "position" |> to_int |> Position.of_int_exn in
        single_symbol json, position)
    in
    Hello symbols
  | "open"  -> Open  (symbol_set json)
  | "close" -> Close (symbol_set json)
  | "error" -> Error (error      json)
  | "book"  ->
    let parse_one_side json =
      json
      |> to_list
      |> List.map ~f:(fun level ->
        match to_list level with
        | [ price; size ] ->
          Price.of_int_exn (to_int price), Size.of_int_exn (to_int size)
        | _ -> raise_s [%message "Ill formatted level in book message" message])
    in
    let buy  = json |> member "buy"  |> parse_one_side in
    let sell = json |> member "sell" |> parse_one_side in
    Book { symbol = single_symbol json; book = Dirpair.create ~buy ~sell }
  | "trade"  -> Trade { symbol = single_symbol json; price = price json; size = size json }
  | "ack"    -> Ack (order_id json)
  | "reject" -> Reject { order_id = order_id json; error = error json }
  | "fill"   ->
    let dir = json |> member "dir" |> to_string |> Dir.of_string_exn in
    Fill
      { order_id = order_id      json
      ; symbol   = single_symbol json
      ; dir
      ; price    = price         json
      ; size     = size          json
      }
  | "out" -> Out (order_id json)
  | s     -> failwithf "Invalid message type: %s" s ()
;;

let%test_module "Message deserialization tests" =
  (module struct
    let%expect_test "HELLO" =
      let message =
        {|{"type":"hello","symbols":[{"symbol":"BOND","position":0},{"symbol":"GS","position":0},{"symbol":"MS","position":0},{"symbol":"USD","position":0},{"symbol":"VALBZ","position":0},{"symbol":"VALE","position":0},{"symbol":"WFC","position":0},{"symbol":"XLF","position":0}]}|}
      in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect
        {| (Hello ((BOND 0) (GS 0) (MS 0) (USD 0) (VALBZ 0) (VALE 0) (WFC 0) (XLF 0))) |}]
    ;;

    let%expect_test "OPEN" =
      let message =
        {|{"type":"open","symbols":["BOND","GS","MS","VALBZ","VALE","WFC","XLF"]}|}
      in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect {| (Open (BOND GS MS VALBZ VALE WFC XLF)) |}]
    ;;

    let%expect_test "CLOSE" =
      let message =
        {|{"type":"close","symbols":["BOND","GS","MS","VALBZ","VALE","WFC","XLF"]}|}
      in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect {| (Close (BOND GS MS VALBZ VALE WFC XLF)) |}]
    ;;

    let%expect_test "ERROR" =
      let message =
        {|{"type":"error","error":"PROTOCOL_ERROR:MALFORMED unknown verb"}|}
      in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect {| (Error "PROTOCOL_ERROR:MALFORMED unknown verb") |}]
    ;;

    let%expect_test "BOOK" =
      let message =
        {|{"type":"book","symbol":"XLF","buy":[[4285,5],[4257,1]],"sell":[[4333,7],[4342,3],[4343,1],[4348,1],[4351,2],[4356,1],[4360,1],[4364,2]]}|}
      in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect
        {|
        (Book
         ((symbol XLF)
          (book
           ((buy ((4285 5) (4257 1)))
            (sell
             ((4333 7) (4342 3) (4343 1) (4348 1) (4351 2) (4356 1) (4360 1)
              (4364 2)))))))
        |}]
    ;;

    let%expect_test "TRADE" =
      let message = {|{"type":"trade","symbol":"MS","price":4106,"size":1}|} in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect {| (Trade ((symbol MS) (price 4106) (size 1))) |}]
    ;;

    let%expect_test "ACK" =
      let message = {|{"type":"ack","order_id":2}|} in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect {| (Ack 2) |}]
    ;;

    let%expect_test "REJECT" =
      let message = {|{"type":"reject","order_id":42,"error":"LIMIT:PRICE_BAND"}|} in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect {| (Reject ((order_id 42) (error LIMIT:PRICE_BAND))) |}]
    ;;

    let%expect_test "FILL" =
      let message =
        {|{"type":"fill","order_id":3,"symbol":"BOND","dir":"SELL","price":999,"size":10}|}
      in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect {| (Fill ((order_id 3) (symbol BOND) (dir Sell) (price 999) (size 10))) |}]
    ;;

    let%expect_test "OUT" =
      let message = {|{"type":"out","order_id":3}|} in
      Core.printf !"%{sexp: t}\n%!" (of_string_exn message);
      [%expect {| (Out 3) |}]
    ;;
  end)
;;
