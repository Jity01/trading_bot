open! Core
open Async
open Import

(* [run_every seconds ~f] is a utility function that will run a given
   function, [f], every [num_seconds] seconds. *)
let run_every seconds ~f =
  Async.Clock.every (Time_float.Span.of_sec seconds) f
;;

type tradable = BOND | VALBZ | VALE

type tradable_record = {
  mutable bond : int
  (* ; mutable gs : int
  ; mutable ms : int  *)
  ; mutable valbz : int
  ; mutable vale : int
  (* ; mutable wfc : int
  ; mutable xlf : int *)
}

module State_manager = struct
  type t = {
    positions : tradable_record
    ; fair_values : tradable_record
    ; exchange_driver : Exchange_driver.t
    ; order_id_generator : Order_id_generator.t
  }

  let is_vale_underpriced t = t.fair_values.vale < t.fair_values.valbz
  let is_vale_overpriced t = t.fair_values.vale > t.fair_values.valbz

  let has_hit_pos_limit t (tradeable : tradable) =
    match tradeable with
    | VALE -> t.positions.vale <= 100 && t.positions.vale >= -100
    | VALBZ -> t.positions.valbz <= 100 && t.positions.valbz >= -100
    | BOND -> t.positions.bond <= 100 && t.positions.bond >= -100
  
    let add_buy_order t (tradeable : tradable) (price : int) (size : int) =
    (* if i cant buy more vale (maybe i can profit by conversion) -> convert ALL vale shares to valbz, sell valbz, and then buy more vale
      (extra: you can calculate diff in holdings and see if it beats the $10 fee & see if we have to do this at all)
      i can buy vale -> buy vale (in little orders) *)
    match tradeable with
    | VALE ->
      (match has_hit_pos_limit t VALE with
      | true -> ()
      | false ->
        Exchange_driver.add_order
        t.exchange_driver
        ~order_id:(Order_id_generator.next_id t.order_id_generator)
        ~symbol:Symbol.vale
        ~dir:Buy
        ~price:(Price.of_int_exn price)
        ~size:(Size.of_int_exn size)
      |> don't_wait_for;)
    | _ -> ()
  let add_sell_order (tradable : tradable) = ()
  let hedge_buy tradable = ()
  let hedge_sell tradable = ()
  let cross_order t (bids : (Price.t, Size.t) list) asks (tradable : tradable) =
    match tradable with
    | VALE ->
      (* if valbz fair value is greater than vale fair value ->
            if i cant buy more vale (maybe i can profit by conversion) -> convert ALL vale shares to valbz, sell valbz, and then buy more vale
            (extra: you can calculate diff in holdings and see if it beats the $10 fee & see if we have to do this at all)
            i can buy vale -> buy vale (in little orders) *)
          (* if valbz fair value is less than vale fair value ->
            if i cant sell more vale (maybe i can profit by conversion) -> buy ALL valbz (within pos limit), convert to vale, sell more vale
            (extra: you can calculate diff in holdings and see if it beats the $10 fee & see if we have to do this at all)
            i can sell vale -> sell vale (in little orders)
          *)
          (match is_vale_underpriced t with
          | true -> add_buy_order VALE; hedge_buy VALE;
          | false -> (
            match is_vale_overpriced t with
            | true -> add_sell_order VALE; hedge_sell VALE;
            | false -> () (* TODO : see if there's anything you can do to make money here. sth to do w conversions ?? *)
          ))
    | _ -> ()
end

(* TODO: track open orders *) 
(* TODO: weighted mid, go down in the books by the quantity you want to trade *)
(* TODO: implement hedging (buy vale @ 91 and to hedge sell @ 99) *)
(* use BBO to place orders instead of using a hardcoded price + fade prices *)

let (state_manager : State_manager) = {
  positions = { bonds = 0 ; valbz = 0 ; vale = 0 }
  ; fair_values = { bonds = 0 ; valbz = 0 ; vale = 0 }
}

let get_weighted_fair_value bids asks = 0
let should_convert_from_valebz_to_vale fv_valebz fv_vale pos_valbz pos_vale =
  not (Int.equal fv_vale fv_valebz)

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
      let order_id_generator = Order_id_generator.create () in
      let latest_order = ref None in
      (* get your positions *)
      (* tak #2 -> each time you're notified of a new entry , do reasonable
         trades *)
      (* pro -> every 5 secs, file a new reasonable order *)
      let read_messages_and_do_some_stuff () =
        (* Read the messages from the exchange. In general, a good rule of
           thumb is to read a LOT more than you write messages to the
           exchange. (Can you see why? Feel free to ask a TA if you're not
           sure.)

           [iter_without_pushback] is a way of reminding us that the stuff we
           do while reading the message probably shouldn't cause us to slow
           down reading, but feel free to change it if you would like. *)
        Async.Pipe.iter_without_pushback exchange_messages ~f:(fun message ->
          (* This is only an example of what you might want to do when you
             see a message from the exchange. You should change this, and
             definitely don't feel constrained to writing code that looks
             similar to what's written here! *)
          match message with
          | Open _ ->
            (match curr_positions.bonds with
             | quant when quant <= 50 ->
               Exchange_driver.add_order
                 exchange_driver
                 ~order_id:(Order_id_generator.next_id order_id_generator)
                 ~symbol:Symbol.bond
                 ~dir:Buy
                 ~price:(Price.of_int_exn 999)
                 ~size:(Size.of_int_exn 50)
               |> don't_wait_for;
               curr_positions.bonds <- curr_positions.bonds + 50
             | _ -> ());
            (match curr_positions.bonds with
             | quant when quant >= -50 ->
               Exchange_driver.add_order
                 exchange_driver
                 ~order_id:(Order_id_generator.next_id order_id_generator)
                 ~symbol:Symbol.bond
                 ~dir:Sell
                 ~price:(Price.of_int_exn 1001)
                 ~size:(Size.of_int_exn 50)
               |> don't_wait_for;
               curr_positions.bonds <- curr_positions.bonds - 50
             | _ -> ())
          | Hello my_positions ->
            List.iter my_positions ~f:(fun pos ->
              let symbol, position = pos in
              match Symbol.to_string symbol, position with
              | "BOND", quantity_of_bonds ->
                curr_positions.bonds
                <- curr_positions.bonds + Position.to_int quantity_of_bonds
              | _ -> ())
          | Fill fill ->
            let new_bids_and_asks = match fill.dir with | Buy -> curr_positions.bonds + Size.to_int fill.size | Sell -> curr_positions.bonds - Size.to_int fill.size in
            (match new_bids_and_asks >= -100 && new_bids_and_asks <= 100 with
            | true ->
            (match Symbol.to_string fill.symbol with
             | "BOND" ->
               Exchange_driver.add_order
                 exchange_driver
                 ~order_id:(Order_id_generator.next_id order_id_generator)
                 ~symbol:Symbol.bond
                 ~dir:fill.dir
                 ~price:fill.price
                 ~size:fill.size
               |> don't_wait_for;
               curr_positions.bonds <- new_bids_and_asks
             | _ -> ())
             | false -> ())
          | Reject _ -> printf !"%{sexp: Exchange_message.t}\n%!" message 
          (* 
            if fair values are the same -> look at the differences in spread (convert to whatever has the bigger spread and trade on that to make money by narrowing the spread)
            and the spreads are not equal (and beyond a certain threshold) ->  *)
          (*  *)
            | Book book ->
            (match Symbol.to_string book.symbol with
             | "BOND" -> ()
             | "VALEBZ" ->
              let bids, asks = book.book.buy, book.book.sell in
               let new_vale_fv = get_weighted_fair_value bids asks in
               fair_values.vale <- new_vale_fv;
               state_manager.cross_order.vale bids asks;
             | "VALE" -> ()
               (* let top_bid, top_ask = List.nth bids 0, List.nth asks 0 in
               (match top_bid, top_ask with
                | ( Some (top_bid_price, top_bid_quantity)
                  , Some (top_ask_price, top_ask_quantity) ) ->
                  fair_values.vale <- 
                  (match Price.to_int top_ask_price < 1000 with
                   | true ->
                     let order_id =
                       Order_id_generator.next_id order_id_generator
                     in
                     Exchange_driver.add_order
                       exchange_driver
                       ~order_id
                       ~symbol:Symbol.bond
                       ~dir:Buy
                       ~price:(Price.of_int_exn (Price.to_int top_bid_price))
                       ~size:(Size.of_int_exn (Size.to_int top_bid_quantity))
                     |> don't_wait_for
                   | false -> ())
                | _ -> ()) *)
             | _ -> ())
          | Close _ -> assert false
          | _ -> ())
        (* Regardless of what the message is, print it. You probably won't
           actually want to print all of the message you see from the
           exchange once you are running your bot in production, because
           there's a lot of messages! *)
        (* Core.printf !"%{sexp: Exchange_message.t}\n%!" message) *)
      in
      let schedule_periodic_nonsense () =
        run_every 1.0 ~f:(fun () ->
          (* Cancel the most recent order, if it exists. *)
          (match !latest_order with
           | None -> ()
           | Some order_id ->
             Exchange_driver.cancel exchange_driver ~order_id
             |> don't_wait_for);
          (* Send a new order. *)
          let next_order_id =
            Order_id_generator.next_id order_id_generator
          in
          Exchange_driver.add_order
            exchange_driver
            ~order_id:next_order_id
            ~symbol:Symbol.bond
            ~dir:Buy
            ~price:(Price.of_int_exn 950)
            ~size:(Size.of_int_exn 1)
          |> don't_wait_for;
          latest_order := Some next_order_id)
      in
      schedule_periodic_nonsense ();
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
