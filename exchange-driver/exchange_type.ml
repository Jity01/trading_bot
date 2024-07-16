open! Core
open! Async

module Test_exchange_type = struct
  type t =
    | Prod_like
    | Slower
    | Empty
  [@@deriving enumerate, sexp_of]

  let port_offset = function
    | Prod_like -> 0
    | Slower    -> 1
    | Empty     -> 2
  ;;
end

type t =
  | Testing of Test_exchange_type.t
  | Prod
[@@deriving enumerate, sexp_of, variants]

let hostname_and_port = function
  | Testing test_exchange ->
    ( Constants.test_exchange_host
    , Constants.test_prod_like_port + Test_exchange_type.port_offset test_exchange )
  | Prod -> Constants.production_host, Constants.production_port
;;

let param =
  let open Command.Param in
  choose_one
    ~if_nothing_chosen:Raise
    [ flag
        "-connect-to-prod"
        (no_arg_some Prod)
        ~doc:"Connect to the prod exchange. Be careful enabling this!"
    ; Enum.make_param
        ~f:optional
        "-connect-to-test"
        (module Test_exchange_type)
        ~doc:"Connect to the specified test exchange."
      |> Command.Param.map ~f:(Option.map ~f:testing)
    ]
;;
