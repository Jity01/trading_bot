open Core

module type Stringable = sig
  type t [@@deriving sexp, compare, bin_io]

  val to_string : t -> string
  val of_string_exn : string -> t

  include Comparable.S_binable with type t := t
  include Hashable.S with type t := t
end

module type Intable = sig
  type t [@@deriving sexp, compare, bin_io]

  val to_int : t -> int
  val of_int_exn : int -> t

  include Stringable with type t := t
end

module Dir = struct
  module T = struct
    type t =
      | Buy
      | Sell
    [@@deriving sexp, compare, bin_io, hash]
  end

  include T
  include Comparable.Make_binable (T)
  include Hashable.Make (T)

  let of_string_exn = function
    | "BUY" -> Buy
    | "SELL" -> Sell
    | s -> raise_s [%message "Invalid direction" s]
  ;;

  let to_string = function Buy -> "BUY" | Sell -> "SELL"
end

module Dirpair = struct
  type 'a t =
    { buy : 'a
    ; sell : 'a
    }
  [@@deriving sexp, compare, bin_io]

  let create ~buy ~sell = { buy; sell }
end

module Uppercase_alpha_string (A : sig
    val max_length : int
    val error : string
  end) =
struct
  let is_upper_alpha = String.for_all ~f:Char.is_uppercase

  (* uppercase alpha only *)
  module T1 = struct
    type t = string [@@deriving compare, hash]

    let to_string s = s

    let of_string s =
      if (not (is_upper_alpha s))
         || String.is_empty s
         || String.length s > A.max_length
      then Or_error.error_string A.error
      else Ok s
    ;;

    let of_string_exn s = Or_error.ok_exn (of_string s)
    let to_sexpable = to_string
    let of_sexpable = of_string_exn
    let to_binable = to_string
    let of_binable = of_string_exn

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "0e37ee06-1510-11ea-8d71-362b9e155667"
    ;;
  end

  module T2 = struct
    include T1
    include Sexpable.Of_sexpable (String) (T1)
    include Binable.Of_binable_with_uuid (String) (T1)
  end

  include T2
  include Comparable.Make_binable (T2)
  include Hashable.Make (T2)
end

module Restricted_integer (A : sig
    val test : int -> bool
    val error : string
  end) =
struct
  module T1 = struct
    type t = int [@@deriving sexp_of, compare, hash]

    let of_int t =
      if not (A.test t) then Or_error.error_string A.error else Ok t
    ;;

    let of_int_exn t = Or_error.ok_exn (of_int t)
    let to_int t = t
    let t_of_sexp s = of_int_exn (Int.t_of_sexp s)
    let to_binable = to_int
    let of_binable = of_int_exn

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "238184de-1510-11ea-8d71-362b9e155667"
    ;;
  end

  module T2 = struct
    include T1
    include Binable.Of_binable_with_uuid (Int) (T1)
  end

  module T3 = struct
    include T2
    include Comparable.Make_binable (T2)
    include Hashable.Make (T2)
  end

  include T3

  let to_string t = Int.to_string (to_int t)

  let of_string s =
    let open Result.Monad_infix in
    (try Ok (Int.of_string s) with _ -> Or_error.error_string A.error)
    >>= of_int
  ;;

  let of_string_exn s = Or_error.ok_exn (of_string s)
end

module Price = struct
  include Restricted_integer (struct
      let test t = t > 0
      let error = "bad price"
    end)
end

module Size = struct
  include Restricted_integer (struct
      let test t = t > 0
      let error = "bad size"
    end)
end

module Order_id = Restricted_integer (struct
    let test t = t >= 0
    let error = "bad order ID"
  end)

module Position = struct
  let check_preempt_overflow x = x > 1 lsl 60 || x < -(1 lsl 60)

  include Restricted_integer (struct
      let test x = not (check_preempt_overflow x)
      let error = "position failed overflow check"
    end)
end

module Symbol = struct
  include Uppercase_alpha_string (struct
      let max_length = 12
      let error = "bad symbol"
    end)

  let bond = of_string_exn "BOND"
  let vale = of_string_exn "VALE"
  let valbz = of_string_exn "VALBZ"
  let xlf = of_string_exn "XLF"
  let gs = of_string_exn "GS"
  let ms = of_string_exn "MS"
  let wfc = of_string_exn "WFC"
end

module Team_name = struct
  include Uppercase_alpha_string (struct
      let max_length = 32
      let error = "team name not an uppercase alpha string"
    end)
end
