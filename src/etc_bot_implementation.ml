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

module Tradeable = struct
  type t =
  | BOND
  | VALBZ
  | VALE
  | XLF
  | GS
  | MS
  | WFC
[@@deriving sexp, equal]
end

let symbol_to_tradable (sym : Symbol.t) =
  match Symbol.to_string sym with
  | "BOND" -> Tradeable.BOND
  | "VALBZ" -> VALBZ
  | "VALE" -> VALE
  | "XLF" -> XLF
  | "GS" -> GS
  | "MS" -> MS
  | _ -> WFC

let tradable_to_symbol (trading : Tradeable.t) =
  match trading with
  | BOND -> Symbol.of_string_exn "BOND"
  | VALBZ -> Symbol.of_string_exn "VALBZ"
  | VALE -> Symbol.of_string_exn "VALE"
  | XLF -> Symbol.of_string_exn "XLF"
  | GS -> Symbol.of_string_exn "GS"
  | MS -> Symbol.of_string_exn "MS"
  | WFC -> Symbol.of_string_exn "WFC"

type tradable_record =
  { mutable bond : int
  ; mutable valbz : int
  ; mutable vale : int
  ; mutable xlf : int
  ; mutable gs : int
  ; mutable ms : int
  ; mutable wfc : int
  }
[@@deriving sexp]

type buys_sells =
  { mutable buys : Order_id.t list
  ; mutable sells : Order_id.t list
  }

type tradable_record_tpls =
  { mutable bond : buys_sells
  ; mutable valbz : buys_sells
  ; mutable vale : buys_sells
  ; mutable xlf : buys_sells
  ; mutable gs : buys_sells
  ; mutable ms : buys_sells
  ; mutable wfc : buys_sells
  }

type offer = int * int

type best_bid_and_ask =
  { mutable best_bid : offer
  ; mutable best_ask : offer
  }

type tradable_best_bids_asks =
  { mutable bond : best_bid_and_ask
  ; mutable valbz : best_bid_and_ask
  ; mutable vale : best_bid_and_ask
  ; mutable xlf : best_bid_and_ask
  ; mutable gs : best_bid_and_ask
  ; mutable ms : best_bid_and_ask
  ; mutable wfc : best_bid_and_ask
  }

type xlf_props =
  { bond : int
  ; ms : int
  ; gs : int
  ; wfc : int
  }

let (limits : tradable_record) =
  { bond = 10
  ; valbz = 10
  ; vale = 10
  ; xlf = 100
  ; gs = 100
  ; ms = 100
  ; wfc = 100
  }
;;

let xlf_total_securities = 10
let (xlf_proportions : xlf_props) = { bond = 3; ms = 3; gs = 2; wfc = 2 }
let unit_size = 1

let get_lst_without lst ~equal ~without =
  List.filter lst ~f:(fun elt -> not (equal elt without))
;;

module State_manager = struct
  type t =
    { open_orders : tradable_record_tpls
    ; positions : tradable_record
    ; exchange_driver : Exchange_driver.t
    ; order_id_generator : Order_id_generator.t
    ; best_bid_and_ask : tradable_best_bids_asks
    }
  (* TODO : change the prices to floats instead ?? *)

  let get_weighted_best_bid_of_xlf_securities t =
    Int.of_float
      ((Float.of_int (fst t.best_bid_and_ask.wfc.best_bid)
        *. (Float.of_int xlf_proportions.wfc
            /. Float.of_int xlf_total_securities))
       +. (Float.of_int (fst t.best_bid_and_ask.gs.best_bid)
           *. (Float.of_int xlf_proportions.gs
               /. Float.of_int xlf_total_securities))
       +. (Float.of_int (fst t.best_bid_and_ask.ms.best_bid)
           *. (Float.of_int xlf_proportions.ms
               /. Float.of_int xlf_total_securities))
       +. (Float.of_int (fst t.best_bid_and_ask.bond.best_bid)
           *. (Float.of_int xlf_proportions.bond
               /. Float.of_int xlf_total_securities)))
  ;;

  let get_weighted_best_ask_of_xlf_securities t =
    Int.of_float
      ((Float.of_int (fst t.best_bid_and_ask.wfc.best_ask)
        *. (Float.of_int xlf_proportions.wfc
            /. Float.of_int xlf_total_securities))
       +. (Float.of_int (fst t.best_bid_and_ask.gs.best_ask)
           *. (Float.of_int xlf_proportions.gs
               /. Float.of_int xlf_total_securities))
       +. (Float.of_int (fst t.best_bid_and_ask.ms.best_ask)
           *. (Float.of_int xlf_proportions.ms
               /. Float.of_int xlf_total_securities))
       +. (Float.of_int (fst t.best_bid_and_ask.bond.best_ask)
           *. (Float.of_int xlf_proportions.bond
               /. Float.of_int xlf_total_securities)))
  ;;

  let can_tighten_vale_by_buying t =
    fst t.best_bid_and_ask.vale.best_bid
    < fst t.best_bid_and_ask.valbz.best_bid
  ;;

  let can_tighten_vale_by_selling t =
    fst t.best_bid_and_ask.vale.best_ask
    > fst t.best_bid_and_ask.valbz.best_ask
  ;;

  let can_tighten_xlf_by_buying t =
    fst t.best_bid_and_ask.xlf.best_bid
    < get_weighted_best_bid_of_xlf_securities t
  
  let can_tighten_xlf_by_selling t =
    fst t.best_bid_and_ask.xlf.best_ask
    > get_weighted_best_ask_of_xlf_securities t
  ;;

  let is_vale_underpriced t =
    fst t.best_bid_and_ask.vale.best_ask
    < fst t.best_bid_and_ask.valbz.best_bid
  ;;

  let is_vale_overpriced t =
    fst t.best_bid_and_ask.vale.best_bid
    > fst t.best_bid_and_ask.valbz.best_ask
  ;;

  let is_xlf_underpriced t =
    fst t.best_bid_and_ask.xlf.best_ask
    < get_weighted_best_bid_of_xlf_securities t
  ;;

  let is_xlf_overpriced t =
    fst t.best_bid_and_ask.xlf.best_bid
    > get_weighted_best_ask_of_xlf_securities t
  ;;

  let get_best_bid t ~(trading : Tradeable.t) =
    match trading with
    | VALE -> t.best_bid_and_ask.vale.best_bid
    | VALBZ -> t.best_bid_and_ask.valbz.best_bid
    | BOND -> t.best_bid_and_ask.bond.best_bid
    | WFC -> t.best_bid_and_ask.wfc.best_bid
    | XLF -> t.best_bid_and_ask.wfc.best_bid
    | GS -> t.best_bid_and_ask.gs.best_bid
    | MS -> t.best_bid_and_ask.ms.best_bid
  ;;

  let get_best_ask t ~(trading : Tradeable.t) =
    match trading with
    | VALE -> t.best_bid_and_ask.vale.best_ask
    | VALBZ -> t.best_bid_and_ask.valbz.best_ask
    | BOND -> t.best_bid_and_ask.bond.best_ask
    | WFC -> t.best_bid_and_ask.wfc.best_ask
    | XLF -> t.best_bid_and_ask.wfc.best_ask
    | GS -> t.best_bid_and_ask.gs.best_ask
    | MS -> t.best_bid_and_ask.ms.best_ask
  ;;

  let get_top_bid_and_ask
    t
    ~(trading : Tradeable.t)
    ~(bids : (Price.t * Size.t) list)
    ~(asks : (Price.t * Size.t) list)
    =
    match List.nth bids 0, List.nth asks 0 with
    | Some (bid_price, bid_size), Some (ask_price, ask_size) ->
      { best_bid = Price.to_int bid_price, Size.to_int bid_size
      ; best_ask = Price.to_int ask_price, Size.to_int ask_size
      }
    | None, Some (ask_price, ask_size) ->
      { best_bid = get_best_bid t ~trading
      ; best_ask = Price.to_int ask_price, Size.to_int ask_size
      }
    | Some (bid_price, bid_size), None ->
      { best_bid = Price.to_int bid_price, Size.to_int bid_size
      ; best_ask = get_best_ask t ~trading
      }
    | None, None ->
      { best_bid = get_best_bid t ~trading
      ; best_ask = get_best_ask t ~trading
      }
  ;;

  let get_total_position t ~(trading : Tradeable.t) =
    match trading with
    | BOND ->
      t.positions.bond
      + List.length t.open_orders.bond.buys
      - List.length t.open_orders.bond.sells
    | VALBZ ->
      t.positions.valbz
      + List.length t.open_orders.valbz.buys
      - List.length t.open_orders.valbz.sells
    | VALE ->
      t.positions.vale
      + List.length t.open_orders.vale.buys
      - List.length t.open_orders.vale.sells
    | XLF ->
      t.positions.xlf
      + List.length t.open_orders.xlf.buys
      - List.length t.open_orders.xlf.sells
    | WFC ->
      t.positions.wfc
      + List.length t.open_orders.wfc.buys
      - List.length t.open_orders.wfc.sells
    | MS ->
      t.positions.ms
      + List.length t.open_orders.ms.buys
      - List.length t.open_orders.ms.sells
    | GS ->
      t.positions.gs
      + List.length t.open_orders.gs.buys
      - List.length t.open_orders.gs.sells
  ;;

  let has_hit_pos_limit t (tradeable : Tradeable.t) =
    match tradeable with
    | VALE ->
      let curr = get_total_position t ~trading:VALE in
      curr > limits.vale || curr < -1 * limits.vale
    | VALBZ ->
      let curr = get_total_position t ~trading:VALBZ in
      curr > limits.valbz || curr < -1 * limits.valbz
    | BOND ->
      let curr = get_total_position t ~trading:BOND in
      curr > limits.bond || curr < -1 * limits.bond
    | XLF ->
      let curr = get_total_position t ~trading:XLF in
      curr > limits.xlf || curr < -1 * limits.xlf
    | GS ->
      let curr = get_total_position t ~trading:GS in
      curr > limits.gs || curr < -1 * limits.gs
    | MS ->
      let curr = get_total_position t ~trading:MS in
      curr > limits.ms || curr < -1 * limits.ms
    | WFC ->
      let curr = get_total_position t ~trading:WFC in
      curr > limits.wfc || curr < -1 * limits.wfc
  ;;

  let will_hit_pos_limit t ~(trading : Tradeable.t) ~new_tot_pos =
    match trading with
    | VALE -> new_tot_pos > limits.vale || new_tot_pos < -1 * limits.vale
    | VALBZ -> new_tot_pos > limits.valbz || new_tot_pos < -1 * limits.valbz
    | BOND -> new_tot_pos > limits.bond || new_tot_pos < -1 * limits.bond
    | WFC -> new_tot_pos > limits.wfc || new_tot_pos < -1 * limits.wfc
    | GS -> new_tot_pos > limits.gs || new_tot_pos < -1 * limits.gs
    | MS -> new_tot_pos > limits.ms || new_tot_pos < -1 * limits.ms
    | XLF -> new_tot_pos > limits.xlf || new_tot_pos < -1 * limits.xlf
  ;;

  let sum_positions (positions : int list) =
    List.fold positions ~init:0 ~f:(fun acc int -> acc + int)
  ;;

  let get_positions_of ~trading new_positions =
      List.filter_map new_positions ~f:(fun (symbol, position) ->
        match Tradeable.equal trading (symbol_to_tradable symbol) with
        | true -> Some (Position.to_int position)
        | _ -> None)
    
  let update_positions_on_hello t (new_positions : (Symbol.t * Position.t) list) =
    let bond_positions = get_positions_of ~trading:BOND new_positions in
    let vale_positions = get_positions_of ~trading:VALE new_positions in
    let valbz_positions = get_positions_of ~trading:VALBZ new_positions in
    let wfc_positions = get_positions_of ~trading:WFC new_positions in
    let gs_positions = get_positions_of ~trading:GS new_positions in
    let ms_positions = get_positions_of ~trading:MS new_positions in
    let xlf_positions = get_positions_of ~trading:XLF new_positions in
    t.positions.bond <- sum_positions bond_positions;
    t.positions.vale <- sum_positions vale_positions;
    t.positions.valbz <- sum_positions valbz_positions;
    t.positions.wfc <- sum_positions wfc_positions;
    t.positions.gs <- sum_positions gs_positions;
    t.positions.ms <- sum_positions ms_positions;
    t.positions.xlf <- sum_positions xlf_positions;
  ;;

  let execute_unit_order t ~(trading : Tradeable.t) ~price ~dir ~order_id =
    match trading with
    | VALE ->
      Exchange_driver.add_order
        t.exchange_driver
        ~order_id
        ~symbol:Symbol.vale
        ~dir
        ~price
        ~size:(Size.of_int_exn unit_size)
      |> don't_wait_for
    | BOND ->
      Exchange_driver.add_order
        t.exchange_driver
        ~order_id
        ~symbol:Symbol.bond
        ~dir
        ~price
        ~size:(Size.of_int_exn unit_size)
      |> don't_wait_for
    | VALBZ ->
      Exchange_driver.add_order
        t.exchange_driver
        ~order_id
        ~symbol:Symbol.valbz
        ~dir
        ~price
        ~size:(Size.of_int_exn unit_size)
      |> don't_wait_for
    | XLF ->
      Exchange_driver.add_order
      t.exchange_driver
      ~order_id
      ~symbol:Symbol.xlf
      ~dir
      ~price
      ~size:(Size.of_int_exn unit_size)
    |> don't_wait_for
    | MS ->
      Exchange_driver.add_order
      t.exchange_driver
      ~order_id
      ~symbol:Symbol.ms
      ~dir
      ~price
      ~size:(Size.of_int_exn unit_size)
    |> don't_wait_for
    | GS ->
      Exchange_driver.add_order
      t.exchange_driver
      ~order_id
      ~symbol:Symbol.gs
      ~dir
      ~price
      ~size:(Size.of_int_exn unit_size)
    |> don't_wait_for
    | WFC ->
      Exchange_driver.add_order
      t.exchange_driver
      ~order_id
      ~symbol:Symbol.wfc
      ~dir
      ~price
      ~size:(Size.of_int_exn unit_size)
    |> don't_wait_for
  ;;

  let update_open_orders_on_buy_order t ~(trading : Tradeable.t) ~order_id =
    match trading with
    | VALE ->
      t.open_orders.vale.buys
      <- List.append t.open_orders.vale.buys [ order_id ]
    | VALBZ ->
      t.open_orders.valbz.buys
      <- List.append t.open_orders.valbz.buys [ order_id ]
    | BOND ->
      t.open_orders.bond.buys
      <- List.append t.open_orders.bond.buys [ order_id ]
    | XLF ->
      t.open_orders.xlf.buys
      <- List.append t.open_orders.xlf.buys [ order_id ]
    | GS ->
      t.open_orders.gs.buys
      <- List.append t.open_orders.gs.buys [ order_id ]
    | MS ->
      t.open_orders.ms.buys
      <- List.append t.open_orders.ms.buys [ order_id ]
    | WFC ->
      t.open_orders.wfc.buys
      <- List.append t.open_orders.wfc.buys [ order_id ]
  ;;

  let update_open_orders_on_sell_order t ~(trading : Tradeable.t) ~order_id =
    match trading with
    | VALE ->
      t.open_orders.vale.sells
      <- List.append t.open_orders.vale.sells [ order_id ]
    | VALBZ ->
      t.open_orders.valbz.sells
      <- List.append t.open_orders.valbz.sells [ order_id ]
    | BOND ->
      t.open_orders.bond.sells
      <- List.append t.open_orders.bond.sells [ order_id ]
    | XLF ->
      t.open_orders.xlf.sells
      <- List.append t.open_orders.xlf.sells [ order_id ]
    | WFC ->
      t.open_orders.wfc.sells
      <- List.append t.open_orders.wfc.sells [ order_id ]
    | GS ->
      t.open_orders.gs.sells
      <- List.append t.open_orders.gs.sells [ order_id ]
    | MS ->
      t.open_orders.ms.sells
      <- List.append t.open_orders.ms.sells [ order_id ]
  ;;

  let execute_unit_buy t ~trading ~price =
    let order_id = Order_id_generator.next_id t.order_id_generator in
    execute_unit_order t ~trading ~price ~dir:Buy ~order_id;
    update_open_orders_on_buy_order t ~trading ~order_id
  ;;

  let execute_unit_sell t ~trading ~price =
    let order_id = Order_id_generator.next_id t.order_id_generator in
    execute_unit_order t ~trading ~price ~dir:Sell ~order_id;
    update_open_orders_on_sell_order t ~trading ~order_id
  ;;

  let add_buy_order
    t
    ~(trading : Tradeable.t)
    ~(price : Price.t)
    ~(size : Size.t)
    =
    (* buy one by one, however many of the given size we can given our
       current positions and update open orders accordingly *)
    let buy_lst = List.init (Size.to_int size) ~f:Fn.id in
    List.iter buy_lst ~f:(fun _ ->
      let curr_tot_pos = get_total_position t ~trading in
      match
        will_hit_pos_limit t ~trading ~new_tot_pos:(curr_tot_pos + unit_size)
      with
      | true -> ()
      | false -> execute_unit_buy t ~trading ~price)
  ;;

  let add_sell_order
    t
    ~(trading : Tradeable.t)
    ~(price : Price.t)
    ~(size : Size.t)
    =
    (* sell one by one, however many of the given size we can given our
       current positions and update open orders accordingly *)
    let sell_lst = List.init (Size.to_int size) ~f:Fn.id in
    List.iter sell_lst ~f:(fun _ ->
      let curr_tot_pos = get_total_position t ~trading in
      match
        will_hit_pos_limit t ~trading ~new_tot_pos:(curr_tot_pos - unit_size)
      with
      | true -> ()
      | false -> execute_unit_sell t ~trading ~price)
  ;;

  let add_order_at_open t = ()
    (* add_buy_order
      t
      ~trading:BOND
      ~price:(Price.of_int_exn 999)
      ~size:(Size.of_int_exn 50); *)
    (* add_sell_order
      t
      ~trading:BOND
      ~price:(Price.of_int_exn 1001)
      ~size:(Size.of_int_exn 50) *)
  ;;

  let convert_etf_to_its_securities t ~(etf : Tradeable.t) =
    let order_id = Order_id_generator.next_id t.order_id_generator in
    match t.positions.vale > 0 with
    | true ->
      Exchange_driver.convert
        t.exchange_driver
        ~order_id
        ~symbol:(tradable_to_symbol VALE)
        ~dir:Sell
        ~size:(Size.of_int_exn t.positions.vale)
      |> don't_wait_for;
      t.positions.valbz <- t.positions.vale + t.positions.valbz;
      t.positions.vale <- 0
    | false ->
      Exchange_driver.convert
        t.exchange_driver
        ~order_id
        ~symbol:(Symbol.of_string_exn "VALE")
        ~dir:Buy
        ~size:(Size.of_int_exn (-1 * t.positions.vale))
      |> don't_wait_for;
      t.positions.valbz <- t.positions.vale + t.positions.valbz;
      t.positions.vale <- 0
  ;;

  let fade_buy t ~(trading : Tradeable.t) ~price_start ~price_end =
    let buy_lst = List.init (price_end - price_start) ~f:Fn.id in
    List.iter buy_lst ~f:(fun price_diff ->
      add_buy_order
        t
        ~trading
        ~price:(Price.of_int_exn (price_start + price_diff + 1))
        ~size:(Size.of_int_exn unit_size))
  ;;

  let fade_sell t ~(trading : Tradeable.t) ~price_start ~price_end =
    let sell_lst = List.init (price_end - price_start) ~f:Fn.id in
    List.iter sell_lst ~f:(fun price_diff ->
      add_sell_order
        t
        ~trading
        ~price:(Price.of_int_exn (price_start + price_diff + 1))
        ~size:(Size.of_int_exn unit_size))
  ;;

  let cross_order t ~bids ~asks ~(trading : Tradeable.t) =
    match trading with
    | VALE ->
      (* vale is underpriced. willing to buy vale @ ask price and hedge sell
         valbz @ buy price. *)
      (match is_vale_underpriced t with
       | true ->
         (* print_endline (Printf.sprintf "vale is underpriced. vale fv: %i.
            valbz fv: %i\n" t.fair_values.vale t.fair_values.valbz ); *)
         (match has_hit_pos_limit t VALE with
          | true -> convert_etf_to_its_securities t ~etf:VALE
          | false -> ());
         let price, size = t.best_bid_and_ask.vale.best_ask in
         add_buy_order
           t
           ~trading:VALE
           ~price:(Price.of_int_exn price)
           ~size:(Size.of_int_exn size)
       | false ->
         (* vale is overpriced. willing to sell vale @ bid price and hedge
            buy valbz @ ask price. *)
         (match is_vale_overpriced t with
          | true ->
            (* print_endline (Printf.sprintf "vale is overpriced. vale fv:
               %i. valbz fv: %i\n" t.fair_values.vale t.fair_values.valbz
               ); *)
            (match has_hit_pos_limit t VALE with
             | true -> convert_etf_to_its_securities t ~etf:VALE
             | false -> ());
            let price, size = t.best_bid_and_ask.vale.best_bid in
            add_sell_order
              t
              ~trading:VALE
              ~price:(Price.of_int_exn price)
              ~size:(Size.of_int_exn size)
          | false ->
            (match can_tighten_vale_by_buying t with
             | false -> ()
             | true ->
               fade_buy
                 t
                 ~trading:VALE
                 ~price_start:(fst t.best_bid_and_ask.vale.best_bid)
                 ~price_end:(fst t.best_bid_and_ask.valbz.best_bid));
            (match can_tighten_vale_by_selling t with
             | false -> ()
             | true ->
               fade_sell
                 t
                 ~trading:VALE
                 ~price_start:(fst t.best_bid_and_ask.valbz.best_ask)
                 ~price_end:(fst t.best_bid_and_ask.vale.best_ask))))
    | XLF ->
      (* xlf is underpriced. willing to buy xlf @ ask price and hedge sell
         its underlying securities @ their respective buy prices. *)
         (match is_xlf_underpriced t with
         | true ->
           (match has_hit_pos_limit t XLF with
            | true -> convert_etf_to_its_securities t ~etf:XLF
            | false -> ());
           let price, size = t.best_bid_and_ask.xlf.best_ask in
           add_buy_order
             t
             ~trading:XLF
             ~price:(Price.of_int_exn price)
             ~size:(Size.of_int_exn size)
         | false ->
           (* xlf is overpriced. willing to sell xlf @ bid price and hedge
              buy its underlying securities @ their respective ask prices. *)
           (match is_xlf_overpriced t with
            | true ->
              (* print_endline (Printf.sprintf "vale is overpriced. vale fv:
                 %i. valbz fv: %i\n" t.fair_values.vale t.fair_values.valbz
                 ); *)
              (match has_hit_pos_limit t XLF with
               | true -> convert_etf_to_its_securities t ~etf:XLF
               | false -> ());
              let price, size = t.best_bid_and_ask.xlf.best_bid in
              add_sell_order
                t
                ~trading:XLF
                ~price:(Price.of_int_exn price)
                ~size:(Size.of_int_exn size)
            | false ->
              (match can_tighten_xlf_by_buying t with
               | false -> ()
               | true ->
                 fade_buy
                   t
                   ~trading:XLF
                   ~price_start:(fst t.best_bid_and_ask.xlf.best_bid)
                   ~price_end:(get_weighted_best_bid_of_xlf_securities t));
              (match can_tighten_vale_by_selling t with
               | false -> ()
               | true ->
                 fade_sell
                   t
                   ~trading:XLF
                   ~price_start:(fst t.best_bid_and_ask.xlf.best_ask)
                   ~price_end:(get_weighted_best_ask_of_xlf_securities t))))
    | _ -> ()
  ;;

  let remove_open_order t ~order_id =
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
         ~without:order_id;
    t.open_orders.wfc.buys
    <- get_lst_without
         t.open_orders.wfc.buys
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.wfc.sells
    <- get_lst_without
         t.open_orders.wfc.sells
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.xlf.buys
    <- get_lst_without
         t.open_orders.xlf.buys
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.xlf.sells
    <- get_lst_without
         t.open_orders.xlf.sells
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.ms.buys
    <- get_lst_without
         t.open_orders.ms.buys
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.ms.sells
    <- get_lst_without
         t.open_orders.ms.sells
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.gs.buys
    <- get_lst_without
         t.open_orders.gs.buys
         ~equal:Order_id.equal
         ~without:order_id;
    t.open_orders.gs.sells
    <- get_lst_without
         t.open_orders.gs.sells
         ~equal:Order_id.equal
         ~without:order_id;
  ;;

  let cancel_order t ~order_id =
    Exchange_driver.cancel t.exchange_driver ~order_id |> don't_wait_for;
    remove_open_order t ~order_id
  ;;
  let get_pos_diff_by_dir ~diff ~(dir : Dir.t) =
    match dir with Sell -> -1 * diff | Buy -> diff
  ;;

  let update_positions_on_fill t ~trading ~(fill : Exchange_message.Fill.t) =
    let pos_diff =
      get_pos_diff_by_dir ~diff:(Size.to_int fill.size) ~dir:fill.dir
    in
    match trading with
    | "VALE" -> t.positions.vale <- t.positions.vale + pos_diff
    | "VALBZ" -> t.positions.valbz <- t.positions.valbz + pos_diff
    | "BOND" -> t.positions.bond <- t.positions.bond + pos_diff
    | _ -> ()
  ;;

  let hedge_vale_on_fill t (fill : Exchange_message.Fill.t) =
    match fill.dir with
    | Buy ->
      (* hedge sell valbz *)
      add_sell_order
        t
        ~trading:VALBZ
        ~price:(Price.of_int_exn (fst t.best_bid_and_ask.valbz.best_bid))
        ~size:fill.size
    | Sell ->
      (* hedge buy valbz *)
      add_buy_order
        t
        ~trading:VALBZ
        ~price:(Price.of_int_exn (fst t.best_bid_and_ask.valbz.best_ask))
        ~size:fill.size
  ;;

  let refill_bond_requests_on_fill t (fill : Exchange_message.Fill.t) =
    match fill.dir with
    | Buy -> add_buy_order t ~trading:BOND ~price:fill.price ~size:fill.size
    | Sell ->
      add_sell_order t ~trading:BOND ~price:fill.price ~size:fill.size
  ;;

  let handle_fill t (fill : Exchange_message.Fill.t) =
    remove_open_order t ~order_id:fill.order_id;
    update_positions_on_fill t ~trading:(Symbol.to_string fill.symbol) ~fill;
    (* custom logic for bonds (re-filling requests) and vale (hedging through
       valbz) *)
    match Symbol.to_string fill.symbol with
    | "VALE" -> hedge_vale_on_fill t fill
    | "BOND" -> refill_bond_requests_on_fill t fill
    | _ -> ()
  ;;

  let update_best_bid_and_ask_prices
    t
    ~trading
    ~(bids : (Price.t * Size.t) list)
    ~(asks : (Price.t * Size.t) list)
    =
    let top_bid_and_ask = get_top_bid_and_ask t ~trading ~bids ~asks in
    match trading with
    | VALE -> t.best_bid_and_ask.vale <- top_bid_and_ask
    | VALBZ -> t.best_bid_and_ask.valbz <- top_bid_and_ask
    | BOND -> t.best_bid_and_ask.bond <- top_bid_and_ask
    | XLF -> t.best_bid_and_ask.xlf <- top_bid_and_ask
    | WFC -> t.best_bid_and_ask.wfc <- top_bid_and_ask
    | MS -> t.best_bid_and_ask.ms <- top_bid_and_ask
    | GS -> t.best_bid_and_ask.gs <- top_bid_and_ask
  ;;
end

(* TODO: weighted mid, go down in the books by the quantity you want to
   trade *)
(* use BBO to place orders instead of using a hardcoded price + fade
   prices *)

let run exchange_type =
  Exchange_driver.connect_and_run
    exchange_type
    ~f:(fun ~exchange_driver ~exchange_messages ->
      let (state_manager : State_manager.t) =
        { positions = { bond = 0; valbz = 0; vale = 0; gs = 0; ms = 0; wfc = 0; xlf = 0 }
        ; exchange_driver
        ; order_id_generator = Order_id_generator.create ()
        ; open_orders =
            { bond = { buys = []; sells = [] }
            ; vale = { buys = []; sells = [] }
            ; valbz = { buys = []; sells = [] }
            ; xlf = { buys = []; sells = [] }
            ; gs = { buys = []; sells = [] }
            ; ms = { buys = []; sells = [] }
            ; wfc = { buys = []; sells = [] }
            }
        ; best_bid_and_ask =
            { bond = { best_bid = 0, 0; best_ask = 0, 0 }
            ; vale = { best_bid = 0, 0; best_ask = 0, 0 }
            ; valbz = { best_bid = 0, 0; best_ask = 0, 0 }
            ; xlf = { best_bid = 0, 0; best_ask = 0, 0 }
            ; gs = { best_bid = 0, 0; best_ask = 0, 0 }
            ; ms = { best_bid = 0, 0; best_ask = 0, 0 }
            ; wfc = { best_bid = 0, 0; best_ask = 0, 0 }
            }
        }
      in
      let read_messages_and_do_some_stuff () =
        Async.Pipe.iter_without_pushback exchange_messages ~f:(fun message ->
          match message with
          | Close _ -> assert false
          | Hello my_positions ->
            State_manager.update_positions_on_hello state_manager my_positions
          | Reject rej ->
            printf !"%{sexp: Exchange_message.t}\n%!" message;
            State_manager.cancel_order state_manager ~order_id:rej.order_id
          | Fill fill ->
            printf !"%{sexp: Exchange_message.t}\n%!" message;
            State_manager.handle_fill state_manager fill
          | Book book ->
            let bids, asks = book.book.buy, book.book.sell in
            (match symbol_to_tradable book.symbol with
             | BOND -> ()
             | VALBZ ->
               State_manager.update_best_bid_and_ask_prices
                 state_manager
                 ~trading:VALBZ
                 ~bids
                 ~asks
              | WFC ->
               State_manager.update_best_bid_and_ask_prices
                 state_manager
                 ~trading:WFC
                 ~bids
                 ~asks
              | GS ->
               State_manager.update_best_bid_and_ask_prices
                 state_manager
                 ~trading:GS
                 ~bids
                 ~asks
              | MS ->
               State_manager.update_best_bid_and_ask_prices
                 state_manager
                 ~trading:MS
                 ~bids
                 ~asks
              | VALE ->
                State_manager.update_best_bid_and_ask_prices
                  state_manager
                  ~trading:VALE
                  ~bids
                  ~asks;
                State_manager.cross_order
                  state_manager
                  ~trading:VALE
                  ~bids
                  ~asks
              | XLF ->
                State_manager.update_best_bid_and_ask_prices
                  state_manager
                  ~trading:XLF
                  ~bids
                  ~asks;
                State_manager.cross_order
                  state_manager
                  ~trading:XLF
                  ~bids
                  ~asks
              )
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
