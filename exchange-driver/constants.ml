open Core
open Types

(* IMPORTANT: MAKE SURE TO CHANGE [team_name] TO BE YOUR TEAM NAME (IN ALL
   CAPS) *)
module Please_update_team_name_here = struct
  let team_name = "JITY"
end

(* IMPORTANT: CHANGES TO THESE VALUES MAY PREVENT YOUR BOT FROM CONNECTING TO
   THE EXCHANGE. *)
module These_values_should_not_be_changed = struct
  open Please_update_team_name_here

  let test_exchange_host = "test-exch-" ^ String.lowercase team_name
  let test_prod_like_port = 22000
  let production_host = "production"
  let production_port = 25000

  (* Redefine team_name to be of the correct type. *)
  let team_name = Team_name.of_string_exn team_name
end

include These_values_should_not_be_changed
