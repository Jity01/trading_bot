open Core
open Types

type t =
  | Hello   of Team_name.t
  | Add     of Order_id.t * Symbol.t * Dir.t * Price.t * Size.t
  | Convert of Order_id.t * Symbol.t * Dir.t * Size.t
  | Cancel  of Order_id.t

let to_string_json t =
  (match t with
   | Hello team_name                          ->
     `Assoc [ "type", `String "hello"; "team", `String (Team_name.to_string team_name) ]
   | Add (order_id, symbol, dir, price, size) ->
     `Assoc
       [ "type"    , `String "add"
       ; "order_id", `Int    (Order_id.to_int order_id)
       ; "symbol"  , `String (Symbol.to_string symbol)
       ; "dir"     , `String (Dir.to_string dir |> String.uppercase)
       ; "price"   , `Int    (Price.to_int price)
       ; "size"    , `Int    (Size.to_int size)
       ]
   | Convert (order_id, symbol, dir, size) ->
     `Assoc
       [ "type"    , `String "convert"
       ; "order_id", `Int    (Order_id.to_int order_id)
       ; "symbol"  , `String (Symbol.to_string symbol)
       ; "dir"     , `String (Dir.to_string dir |> String.uppercase)
       ; "size"    , `Int    (Size.to_int size)
       ]
   | Cancel order_id ->
     `Assoc [ "type", `String "cancel"; "order_id", `Int (Order_id.to_int order_id) ])
  |> Yojson.Basic.to_string
;;

let%test_module "Serialization tests" =
  (module struct
    let%expect_test "HELLO" =
      let t = Hello (Team_name.of_string_exn "TEST") in
      Core.printf "%s\n%!" (to_string_json t);
      [%expect {| {"type":"hello","team":"TEST"} |}]
    ;;

    let%expect_test "ADD" =
      let t =
        Add
          ( Order_id.of_int_exn 42
          , Symbol.of_string_exn "XLF"
          , Dir.Buy
          , Price.of_int_exn 100
          , Size.of_int_exn 10 )
      in
      Core.printf "%s\n%!" (to_string_json t);
      [%expect
        {| {"type":"add","order_id":42,"symbol":"XLF","dir":"BUY","price":100,"size":10} |}]
    ;;

    let%expect_test "CONVERT" =
      let t =
        Convert
          (Order_id.of_int_exn 42, Symbol.of_string_exn "XLF", Dir.Buy, Size.of_int_exn 10)
      in
      Core.printf "%s\n%!" (to_string_json t);
      [%expect
        {| {"type":"convert","order_id":42,"symbol":"XLF","dir":"BUY","size":10} |}]
    ;;

    let%expect_test "CANCEL" =
      let t = Cancel (Order_id.of_int_exn 42) in
      Core.printf "%s\n%!" (to_string_json t);
      [%expect {| {"type":"cancel","order_id":42} |}]
    ;;
  end)
;;
