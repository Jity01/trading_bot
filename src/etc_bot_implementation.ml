open! Core
open Async
open Import

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

type buys_sells =
  { mutable buys : Order_id.t list
  ; mutable sells : Order_id.t list
  }

type tradable_record_tpls =
  { bond : buys_sells (* ; mutable gs : int ; mutable ms : int *)
  ; valbz : buys_sells
  ; vale : buys_sells (* ; mutable wfc : int ; mutable xlf : int *)
  }

type offer = int * int

type best_bid_and_ask = { mutable best_bid : offer ; mutable best_ask : offer }

type tradable_best_bids_asks =
  {
    mutable bond : best_bid_and_ask
    ; mutable valbz : best_bid_and_ask
    ; mutable vale : best_bid_and_ask
  }

let (limits : tradable_record) = { bond = -10 ; vale = 10 ; valbz = 10}

let get_lst_without lst ~equal ~without =
  List.filter lst ~f:(fun elt -> not (equal elt without))
;;

(* let get_top_price_and_size (lst : (Price.t * Size.t) list) = List.nth lst 0 *)

module State_manager = struct
  type t =
    { open_orders : tradable_record_tpls
    ; positions : tradable_record
    ; fair_values : tradable_record
    ; exchange_driver : Exchange_driver.t
    ; order_id_generator : Order_id_generator.t
    ; best_bid_and_ask : tradable_best_bids_asks
    }

  let is_vale_underpriced t = t.fair_values.vale < t.fair_values.valbz
  let is_vale_overpriced t = t.fair_values.vale > t.fair_values.valbz

  let get_best_bid t ~trading =
    match trading with
    | VALE -> t.best_bid_and_ask.vale.best_bid
    | VALBZ -> t.best_bid_and_ask.valbz.best_bid
    | BOND -> t.best_bid_and_ask.bond.best_bid
  
  let get_best_ask t ~trading =
    match trading with
    | VALE -> t.best_bid_and_ask.vale.best_ask
    | VALBZ -> t.best_bid_and_ask.valbz.best_ask
    | BOND -> t.best_bid_and_ask.bond.best_ask

  let get_top_bid_and_ask_prices t ~(trading : tradable) ~(bids : (Price.t * Size.t) list) ~(asks : (Price.t * Size.t) list) =
    match (List.nth bids 0), (List.nth asks 0) with
    | Some (bid_price, bid_size), Some (ask_price, ask) -> Price.to_int bid_price, Price.to_int ask_price
    | None, Some (ask_price, _) -> get_best_bid t ~trading, Price.to_int ask_price
    | Some (bid_price, _), None -> Price.to_int bid_price, get_best_ask t ~trading
    | None, None -> get_best_bid t ~trading, get_best_ask t ~trading

  let cancel_order t ~order_id =
    Exchange_driver.cancel t.exchange_driver ~order_id |> don't_wait_for;
    t.open_orders.vale.buys
    <- get_lst_without
         t.open_orders.vale.buys
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.vale.sells
    <- get_lst_without
         t.open_orders.vale.sells
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.valbz.buys
    <- get_lst_without
         t.open_orders.valbz.buys
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.valbz.sells
    <- get_lst_without
         t.open_orders.valbz.sells
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.bond.buys
    <- get_lst_without
         t.open_orders.bond.buys
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.bond.sells
    <- get_lst_without
         t.open_orders.bond.sells
         ~equal:Order_id.equal
         ~without:order_id
  ;;

  let get_num_of_filled_and_open_orders t ~trading =
    match trading with
    | BOND -> t.positions.bond + (List.length t.open_orders.bond.buys) - (List.length t.open_orders.bond.sells)
    | VALBZ -> t.positions.valbz + (List.length t.open_orders.valbz.buys) - (List.length t.open_orders.valbz.sells)
    | VALE -> t.positions.vale + (List.length t.open_orders.vale.buys) - (List.length t.open_orders.vale.sells)
  let has_hit_pos_limit t (tradeable : tradable) =
    match tradeable with
    | VALE ->
      let curr = get_num_of_filled_and_open_orders t ~trading:VALE in
      curr > limits.vale || curr < -1 * limits.vale
    | VALBZ ->
      let curr = get_num_of_filled_and_open_orders t ~trading:VALBZ in
      curr > limits.valbz || curr < -1 * limits.valbz
    | BOND ->
      let curr = get_num_of_filled_and_open_orders t ~trading:BOND in
      curr > limits.bond || curr <  -1 * limits.bond
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
         t.open_orders.bond.sells
         <- List.append t.open_orders.bond.sells [ order_id ]
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
         t.open_orders.vale.sells
         <- List.append t.open_orders.vale.sells [ order_id ]
       | true -> ())
    | _ -> ()
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
         t.open_orders.bond.buys
         <- List.append t.open_orders.bond.buys [ order_id ]
       | true -> ())
    | VALE ->
      Exchange_driver.add_order
        t.exchange_driver
        ~order_id:(Order_id_generator.next_id t.order_id_generator)
        ~symbol:Symbol.vale
        ~dir:Buy
        ~price
        ~size
      |> don't_wait_for;
      t.open_orders.vale.buys
      <- List.append t.open_orders.vale.buys [ order_id ]
    | _ -> ()
  ;;

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

  let convert_vale_to_valebz t ~price ~size =
    let order_id = Order_id_generator.next_id t.order_id_generator in
    match t.positions.vale > 0 with
    | true ->
      Exchange_driver.convert
        t.exchange_driver
        ~order_id
        ~symbol:(Symbol.of_string_exn "VALE")
        ~dir:Sell
        ~size:(Size.of_int_exn t.positions.vale)
      |> don't_wait_for;
      t.positions.valbz <- t.positions.valbz + t.positions.vale;
      t.positions.vale <- 0
    | false -> add_buy_order t ~trading:VALE ~price ~size
  ;;

  let cross_order t ~bids ~asks ~(trading : tradable) =
    match trading with
    | VALE ->
      (match is_vale_underpriced t with
       | true ->
          print_endline (Printf.sprintf "vale is underpriced. vale fv: %i. valbz fv: %i\n" t.fair_values.vale t.fair_values.valbz );
          (match has_hit_pos_limit t VALE with
            | true -> convert_vale_to_valebz t ~price ~size
            | false -> ());
          add_buy_order t ~trading:VALE ~price ~size
       | false -> print_endline (Printf.sprintf "vale is underpriced. vale fv: %i. valbz fv: %i\n" t.fair_values.vale t.fair_values.valbz );)
         (* (match is_vale_overpriced t with
          | true ->
            printf "vale is overpriced\n";
            (match get_top_price_and_size asks with
             | None -> ()
             | Some (price, size) ->
               add_sell_order t ~trading:VALE ~price ~size)
          | false ->
            printf "vale is fair\n";
            () *)
            (* TODO : see if there's anything you can do to make money here.
               sth to do w conversions ?? *)
    | _ -> ()
  ;;

  let handle_fill t (fill : Exchange_message.Fill.t) =
    match Symbol.to_string fill.symbol with
    | "VALE" ->
      (match fill.dir with
       | Buy ->
         t.open_orders.vale.buys
         <- get_lst_without
              t.open_orders.vale.buys
              ~equal:Order_id.equal
              ~without:fill.order_id;
         t.positions.vale <- t.positions.vale + Size.to_int fill.size;
         add_sell_order t ~trading:VALBZ ~price:(Price.of_int_exn t.fair_values.valbz) ~size:fill.size
       | Sell ->
         t.open_orders.vale.sells
         <- get_lst_without
              t.open_orders.vale.sells
              ~equal:Order_id.equal
              ~without:fill.order_id;
         t.positions.vale <- t.positions.vale - Size.to_int fill.size);
         add_buy_order t ~trading:VALBZ ~price:(Price.of_int_exn t.fair_values.valbz) ~size:fill.size
    | "BOND" ->
      (match fill.dir with
       | Buy ->
         t.open_orders.bond.buys
         <- get_lst_without
              t.open_orders.bond.buys
              ~equal:Order_id.equal
              ~without:fill.order_id;
         t.positions.bond <- t.positions.bond + Size.to_int fill.size;
         add_buy_order t ~trading:BOND ~price:fill.price ~size:fill.size
       | Sell ->
         t.open_orders.bond.sells
         <- get_lst_without
              t.open_orders.bond.sells
              ~equal:Order_id.equal
              ~without:fill.order_id;
         t.positions.bond <- t.positions.bond - Size.to_int fill.size;
         add_sell_order t ~trading:BOND ~price:fill.price ~size:fill.size)
    | "VALBZ" ->
      (match fill.dir with
       | Buy ->
         t.open_orders.valbz.buys
         <- get_lst_without
              t.open_orders.valbz.buys
              ~equal:Order_id.equal
              ~without:fill.order_id;
         t.positions.valbz <- t.positions.valbz + Size.to_int fill.size
       | Sell ->
         t.open_orders.valbz.sells
         <- get_lst_without
              t.open_orders.valbz.sells
              ~equal:Order_id.equal
              ~without:fill.order_id;
         t.positions.valbz <- t.positions.valbz - Size.to_int fill.size)
    | _ -> ()
  ;;
  let get_weighted_fair_value
  t
  ~(trading : tradable)
  ~(bids : (Price.t * Size.t) list)
  ~(asks : (Price.t * Size.t) list)
  =
  match List.is_empty bids || List.is_empty asks with
  | true ->
    (match trading with
     | BOND -> t.fair_values.bond
     | VALE -> t.fair_values.vale
     | VALBZ -> t.fair_values.valbz)
  | false ->
    let (top_bid_price, _), (top_ask_price, _) =
      List.nth_exn bids 0, List.nth_exn asks 0
    in
    (Price.to_int top_bid_price + Price.to_int top_ask_price) / 2
;;

  let update_fair_values t ~trading ~bids ~asks =
    let new_vale_fv = get_weighted_fair_value t ~trading ~bids ~asks in
    match trading with
    | VALBZ -> t.fair_values.valbz <- new_vale_fv
    | VALE -> t.fair_values.vale <- new_vale_fv
    | BOND -> t.fair_values.bond <- new_vale_fv

  let update_best_bid_and_ask_prices t ~trading ~(bids : (Price.t * Size.t) list) ~(asks : (Price.t * Size.t) list) =
    let bid, ask = get_top_bid_and_ask_prices t ~trading ~bids ~asks in
    match trading with
    | VALE ->
      t.best_bid_and_ask.vale.best_bid <- bid;
      t.best_bid_and_ask.vale.best_ask <- ask
    | VALBZ -> 
      t.best_bid_and_ask.valbz.best_bid <- bid;
      t.best_bid_and_ask.valbz.best_ask <- ask
    | BOND ->
      t.best_bid_and_ask.bond.best_bid <- bid;
      t.best_bid_and_ask.bond.best_ask <- ask
end

(* TODO: track open orders *)
(* TODO: weighted mid, go down in the books by the quantity you want to
   trade *)
(* TODO: implement hedging (buy vale @ 91 and to hedge sell @ 99) *)
(* use BBO to place orders instead of using a hardcoded price + fade prices *)

let run exchange_type =
  (* Set up a connection to the exchange. *)
  Exchange_driver.connect_and_run
    exchange_type
    ~f:(fun ~exchange_driver ~exchange_messages ->
      let (state_manager : State_manager.t) =
        { positions = { bond = 0; valbz = 0; vale = 0 }
        ; fair_values = { bond = 1000; valbz = 0; vale = 0 }
        ; exchange_driver
        ; order_id_generator = Order_id_generator.create ()
        ; open_orders =
            { bond = { buys = []; sells = [] }
            ; vale = { buys = []; sells = [] }
            ; valbz = { buys = []; sells = [] }
            }
        ; best_bid_and_ask =
          { bond = { best_bid = 0; best_ask = 0 }
            ; vale = { best_bid = 0; best_ask = 0 }
            ; valbz = { best_bid = 0; best_ask = 0 }
            }
        
        }
      in
      let read_messages_and_do_some_stuff () =
        Async.Pipe.iter_without_pushback exchange_messages ~f:(fun message ->
          match message with
          | Open _ -> State_manager.add_order_at_open state_manager
          | Close _ -> assert false
          | Hello my_positions ->
            State_manager.update_positions state_manager my_positions
          | Reject rej ->
            printf !"%{sexp: Exchange_message.t}\n%!" message;
            State_manager.cancel_order state_manager ~order_id:rej.order_id
          | Fill fill ->
            printf !"%{sexp: Exchange_message.t}\n%!" message;
            State_manager.handle_fill state_manager fill
          | Book book ->
            let bids, asks = book.book.buy, book.book.sell in
            (match Symbol.to_string book.symbol with
             | "VALBZ" ->
               State_manager.update_fair_values state_manager ~trading:VALBZ ~bids ~asks;
               State_manager.update_best_bid_and_ask_prices state_manager ~trading:VALBZ ~bids ~asks
             | "VALE" ->
               State_manager.update_fair_values state_manager ~trading:VALE ~bids ~asks;
               State_manager.update_best_bid_and_ask_prices state_manager ~trading:VALBZ ~bids ~asks;
               State_manager.cross_order state_manager ~trading:VALE ~bids ~asks
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
