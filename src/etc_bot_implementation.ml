open! Core
open Async
open Import

(* let top_bid, top_ask = List.nth bids 0, List.nth asks 0 in (match top_bid,
   top_ask with | ( Some (top_bid_price, top_bid_quantity) , Some
   (top_ask_price, top_ask_quantity) ) -> fair_values.vale <- (match
   Price.to_int top_ask_price < 1000 with | true -> let order_id =
   Order_id_generator.next_id order_id_generator in Exchange_driver.add_order
   exchange_driver ~order_id ~symbol:Symbol.bond ~dir:Buy
   ~price:(Price.of_int_exn (Price.to_int top_bid_price))
   ~size:(Size.of_int_exn (Size.to_int top_bid_quantity)) |> don't_wait_for |
   false -> ()) | _ -> ()) *)
(* Regardless of what the message is, print it. You probably won't actually
   want to print all of the message you see from the exchange once you are
   running your bot in production, because there's a lot of messages! *)
(* Core.printf !"%{sexp: Exchange_message.t}\n%!" message) *)
(* let schedule_periodic_nonsense () = run_every 1.0 ~f:(fun () -> (* Cancel
   the most recent order, if it exists. *) (match !latest_order with | None
   -> () | Some order_id -> Exchange_driver.cancel exchange_driver ~order_id
   |> don't_wait_for); (* Send a new order. *) let next_order_id =
   Order_id_generator.next_id order_id_generator in Exchange_driver.add_order
   exchange_driver ~order_id:next_order_id ~symbol:Symbol.bond ~dir:Buy
   ~price:(Price.of_int_exn 950) ~size:(Size.of_int_exn 1) |> don't_wait_for;
   latest_order := Some next_order_id) *)

(* [run_every seconds ~f] is a utility function that will run a given
   function, [f], every [num_seconds] seconds. *)
let run_every seconds ~f =
  Async.Clock.every (Time_float.Span.of_sec seconds) f
;;

(* if fair values are the same -> look at the differences in spread (convert
   to whatever has the bigger spread and trade on that to make money by
   narrowing the spread) and the spreads are not equal (and beyond a certain
   threshold) -> *)
(* *)

type tradable =
  | BOND
  | VALBZ
  | VALE
[@@deriving sexp]

type tradable_record =
  { mutable bond : int (* ; mutable gs : int ; mutable ms : int *)
  ; mutable valbz : int
  ; mutable vale : int (* ; mutable wfc : int ; mutable xlf : int *)
  }
[@@deriving sexp]

type tradable_record_ids =
  { mutable bond : Order_id.t list
      (* ; mutable gs : int ; mutable ms : int *)
  ; mutable valbz : Order_id.t list
  ; mutable vale : Order_id.t list
  (* ; mutable wfc : int ; mutable xlf : int *)
  }
[@@deriving sexp]

type buys_sells = { mutable buys : Order_id.t list ; mutable sells : Order_id.t list}

type tradable_record_tpls =
{ mutable bond : buys_sells
      (* ; mutable gs : int ; mutable ms : int *)
  ; mutable valbz : buys_sells
  ; mutable vale : buys_sells
  (* ; mutable wfc : int ; mutable xlf : int *)
}

module State_manager = struct
  type t =
    { open_orders : tradable_record_tpls
    ; positions : tradable_record
    ; fair_values : tradable_record
    ; exchange_driver : Exchange_driver.t
    ; order_id_generator : Order_id_generator.t
    }

  let is_vale_underpriced t = t.fair_values.vale < t.fair_values.valbz
  let is_vale_overpriced t = t.fair_values.vale > t.fair_values.valbz

  let has_hit_pos_limit t (tradeable : tradable) =
    match tradeable with
    | VALE -> t.positions.vale > 10 || t.positions.vale < -10
    | VALBZ -> t.positions.valbz > 10 || t.positions.valbz < -10
    | BOND -> t.positions.bond > 100 || t.positions.bond < -100
  ;;

  let add_sell_order
    t
    ~(trading : tradable)
    ~(price : Price.t)
    ~(size : Size.t)
    =
    let order_id = Order_id_generator.next_id t.order_id_generator in
    match trading with
    | BOND ->
      (match has_hit_pos_limit t BOND with
       | false ->
         Exchange_driver.add_order
           t.exchange_driver
           ~order_id
           ~symbol:Symbol.bond
           ~dir:Sell
           ~price
           ~size
         |> don't_wait_for;
         (* t.positions.bond <- t.positions.bond - Size.to_int size; *)
         (* t.order_ids.bond
         <- List.filter t.order_ids.bond ~f:(fun id ->
              not (Order_id.equal id order_id)) *)
          t.open_orders.bond.sells <- List.append t.open_orders.bond.sells [ order_id ]
       | true -> ())
    | VALE ->
      (match has_hit_pos_limit t VALE with
       | false ->
         Exchange_driver.add_order
           t.exchange_driver
           ~order_id
           ~symbol:Symbol.vale
           ~dir:Sell
           ~price
           ~size
         |> don't_wait_for;
         (* t.positions.vale <- t.positions.vale - Size.to_int size;
         t.order_ids.vale
         <- List.filter t.order_ids.vale ~f:(fun id ->
              not (Order_id.equal id order_id)) *)
          t.open_orders.vale.sells <- List.append t.open_orders.vale.sells [ order_id ]
       | true -> ())
    | _ -> ()
  ;;

  let convert_vale_to_valebz t () =
    (* let fair_price, all_size =
      Price.of_int_exn t.fair_values.valbz, Size.of_int_exn t.positions.vale
    in *)
    (* add_sell_order t ~trading:VALBZ ~price:fair_price ~size:all_size; *)
    (* print_s [%sexp (t.positions.vale : int)]; *)
    match t.positions.vale > 0 with
    | true ->
      List.iter t.open_orders.vale ~f:(fun order_id ->
        Exchange_driver.convert
          t.exchange_driver
          ~order_id
          ~symbol:(Symbol.of_string_exn "VALE")
          ~dir:Sell
          ~size:(Size.of_int_exn t.positions.vale)
        |> don't_wait_for;
        t.open_orders.vale.sells <- List.append t.open_orders.vale.sells [ order_id ];
        t.open_orders.valbz.buys <- List.append t.open_orders.vale.buys [ order_id ]
      )
    | false ->
      List.iter t.order_ids.vale ~f:(fun order_id ->
        Exchange_driver.convert
          t.exchange_driver
          ~order_id
          ~symbol:(Symbol.of_string_exn "VALE")
          ~dir:Buy
          ~size:(Size.of_int_exn t.positions.vale)
        |> don't_wait_for;
        t.open_orders.vale.buys <- List.append t.open_orders.vale.buys [ order_id ];
        t.open_orders.valbz.sells <- List.append t.open_orders.vale.sells [ order_id ])
  ;;

  let sum_positions (positions : int list) =
    List.fold positions ~init:0 ~f:(fun acc int -> acc + int)
  ;;

  let update_positions t (new_positions : (Symbol.t * Position.t) list) =
    let bond_positions =
      List.filter_map new_positions ~f:(fun (symbol, position) ->
        match Symbol.to_string symbol with
        | "BOND" -> Some (Position.to_int position)
        | _ -> None)
    in
    let vale_positions =
      List.filter_map new_positions ~f:(fun (symbol, position) ->
        match Symbol.to_string symbol with
        | "VALE" -> Some (Position.to_int position)
        | _ -> None)
    in
    let valbz_positions =
      List.filter_map new_positions ~f:(fun (symbol, position) ->
        match Symbol.to_string symbol with
        | "VALBZ" -> Some (Position.to_int position)
        | _ -> None)
    in
    t.positions.bond <- sum_positions bond_positions;
    t.positions.vale <- sum_positions vale_positions;
    t.positions.valbz <- sum_positions valbz_positions
  ;;

  let add_buy_order
    t
    ~(trading : tradable)
    ~(price : Price.t)
    ~(size : Size.t)
    =
    (* if i cant buy more vale (maybe i can profit by conversion) -> convert
       ALL vale shares to valbz, sell valbz, and then buy more vale (extra:
       you can calculate diff in holdings and see if it beats the $10 fee &
       see if we have to do this at all) i can buy vale -> buy vale (in
       little orders) *)
    let order_id = Order_id_generator.next_id t.order_id_generator in
    match trading with
    | BOND ->
      (match has_hit_pos_limit t BOND with
       | false ->
         Exchange_driver.add_order
           t.exchange_driver
           ~order_id
           ~symbol:Symbol.bond
           ~dir:Buy
           ~price
           ~size
         |> don't_wait_for;
         (* t.positions.bond <- t.positions.bond + Size.to_int size; *)
        t.open_orders.bond.buys <- List.append t.open_orders.bond.buys [ order_id ]
         (* t.order_ids.bond <- List.append t.order_ids.bond [ order_id ] *)
       | true -> ())
    | VALE ->
      (match has_hit_pos_limit t VALE with
       | true -> convert_vale_to_valebz t ()
       | false -> ());
      Exchange_driver.add_order
        t.exchange_driver
        ~order_id:(Order_id_generator.next_id t.order_id_generator)
        ~symbol:Symbol.vale
        ~dir:Buy
        ~price
        ~size
      |> don't_wait_for;
      (* t.positions.vale <- t.positions.bond + Size.to_int size;
      t.order_ids.valbz <- List.append t.order_ids.bond [ order_id ] *)
      t.open_orders.vale.buys <- List.append t.open_orders.vale.buys [ order_id ]
    | _ -> ()
  ;;

  let get_top_price_and_size (lst : (Price.t * Size.t) list) = List.nth lst 0

  (* let hedge_buy ~trading ~price ~size = () let hedge_sell ~trading ~price
     ~size = () *)
  let add_order_at_open t =
    add_buy_order
      t
      ~trading:BOND
      ~price:(Price.of_int_exn 999)
      ~size:(Size.of_int_exn 50);
    add_sell_order
      t
      ~trading:BOND
      ~price:(Price.of_int_exn 1001)
      ~size:(Size.of_int_exn 50)
  ;;

  let cross_order t ~bids ~asks ~(trading : tradable) =
    match trading with
    | VALE ->
      (match is_vale_underpriced t with
       | true ->
         (match get_top_price_and_size bids with
          | None -> ()
          | Some (price, size) -> add_buy_order t ~trading:VALE ~price ~size)
       | false ->
         (match is_vale_overpriced t with
          | true ->
            printf "vale is overpriced\n";
            (match get_top_price_and_size asks with
             | None -> ()
             | Some (price, size) ->
               add_sell_order t ~trading:VALE ~price ~size)
          | false ->
            printf "vale is fair\n";
            ()
            (* TODO : see if there's anything you can do to make money here.
               sth to do w conversions ?? *)))
    | _ -> ()
  ;;
end

(* let update_positions t positions = List.iter my_positions ~f:(fun pos ->
   let symbol, position = pos in match Symbol.to_string symbol, position with
   | "BOND", quantity_of_bonds -> curr_positions.bonds <-
   curr_positions.bonds + Position.to_int quantity_of_bonds | _ -> ()) | Fill
   fill -> let new_bids_and_asks = match fill.dir with | Buy ->
   curr_positions.bonds + Size.to_int fill.size | Sell ->
   curr_positions.bonds - Size.to_int fill.size in (match new_bids_and_asks
   >= -100 && new_bids_and_asks <= 100 with | true -> (match Symbol.to_string
   fill.symbol with | "BOND" -> Exchange_driver.add_order exchange_driver
   ~order_id:(Order_id_generator.next_id order_id_generator)
   ~symbol:Symbol.bond ~dir:fill.dir ~price:fill.price ~size:fill.size |>
   don't_wait_for; curr_positions.bonds <- new_bids_and_asks | _ -> ()) |
   false -> ()) *)

(* TODO: track open orders *)
(* TODO: weighted mid, go down in the books by the quantity you want to
   trade *)
(* TODO: implement hedging (buy vale @ 91 and to hedge sell @ 99) *)
(* use BBO to place orders instead of using a hardcoded price + fade prices *)

let get_weighted_fair_value
  (state_manager : State_manager.t)
  (trading : tradable)
  (bids : (Price.t * Size.t) list)
  (asks : (Price.t * Size.t) list)
  =
  match List.is_empty bids || List.is_empty asks with
  | true ->
    (match trading with
     | BOND -> state_manager.fair_values.bond
     | VALE -> state_manager.fair_values.vale
     | VALBZ -> state_manager.fair_values.valbz)
  | false ->
    let (top_bid_price, _), (top_ask_price, _) =
      List.nth_exn bids 0, List.nth_exn asks 0
    in
    (Price.to_int top_bid_price + Price.to_int top_ask_price) / 2
;;

let should_convert_from_valebz_to_vale fv_valebz fv_vale pos_valbz pos_vale =
  not (Int.equal fv_vale fv_valebz)
;;

(* This is an example of what your ETC bot might look like. You should treat
   this as a starting point, and do not feel beholden to anything that this
   code is doing! Feel free to delete, add, and reorganize code however you
   would like. (Indeed, you should change the logic here! It does not make
   sense!) *)
let run exchange_type =
  (* Set up a connection to the exchange. *)
  Exchange_driver.connect_and_run
    exchange_type
    ~f:(fun ~exchange_driver ~exchange_messages ->
      (* Initiate the [Order_id_generator], which will help us get unique ids
         to attach to the orders we send to the exchange. You don't have to
         use this, but it'll make coming up with valid order ids a bit
         easier. Feel free to open up order_id_generator.ml to see how the
         code works. *)
      let (state_manager : State_manager.t) =
        { positions = { bond = 0; valbz = 0; vale = 0 }
        ; fair_values = { bond = 0; valbz = 0; vale = 0 }
        ; exchange_driver
        ; order_id_generator = Order_id_generator.create ()
        ; open_orders = { bond = { buys = [] ; sells = []} ; vale = { buys = [] ; sells = []} ; valbz = { buys = [] ; sells = []} }
        }
      in
      state_manager.order_ids.valbz |> ignore;
      let read_messages_and_do_some_stuff () =
        Async.Pipe.iter_without_pushback exchange_messages ~f:(fun message ->
          match message with
          | Open _ -> State_manager.add_order_at_open state_manager
          | Close _ -> assert false
          | Hello my_positions ->
            State_manager.update_positions state_manager my_positions
          | Reject rej ->
            (match
               (List.mem
                 state_manager.open_orders.vale.buys
                 rej.order_id
                 ~equal:Order_id.equal) || (List.mem
                 state_manager.open_orders.vale.sells
                 rej.order_id
                 ~equal:Order_id.equal)
             with
             | true -> printf !"rejected vale"
             | false -> printf "rejected bond")
          | Ack _ -> printf !"%{sexp: Exchange_message.t}\n%!" message
          | Fill fill ->
            (match Symbol.to_string fill.symbol with
            | "VALE" -> match )
          | Book book ->
            (match Symbol.to_string book.symbol with
             | "VALBZ" ->
               let bids, asks = book.book.buy, book.book.sell in
               let new_vale_fv =
                 get_weighted_fair_value state_manager VALBZ bids asks
               in
               state_manager.fair_values.valbz <- new_vale_fv
               (* State_manager.narrow_spread state_manager ~bids ~asks *)
             | "VALE" ->
               let bids, asks = book.book.buy, book.book.sell in
               let new_vale_fv =
                 get_weighted_fair_value state_manager VALE bids asks
               in
               state_manager.fair_values.vale <- new_vale_fv;
               print_s [%sexp (state_manager.positions : tradable_record)];
               State_manager.cross_order
                 state_manager
                 ~trading:VALE
                 ~bids
                 ~asks
             | _ -> ())
          | _ -> ())
      in
      read_messages_and_do_some_stuff ())
;;

let command =
  Async.Command.async
    ~summary:"My etc bot"
    (* This bot starts off with flags to choose between available exchanges
       and set which log levels are displayed. Feel free to add any other
       flags you need! *)
    [%map_open.Command
      let exchange_type = Exchange_type.param in
      fun () -> run exchange_type]
;;
