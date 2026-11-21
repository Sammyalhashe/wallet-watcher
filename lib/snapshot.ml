(* The result of one fetch round, before any analytics. *)

(* [usd] is set when the source values the holding itself; otherwise it is
   priced as amount * spot price. Loans have negative [amount] and [usd]. *)
type holding = {
  asset : Config.asset;
  amount : float;
  usd : float option;
  detail : string option; (* e.g. "Aave V3 deposit" *)
}

let holding asset amount = { asset; amount; usd = None; detail = None }

(* What one address holds on one network, or why it failed. *)
type fetch_result = (holding list, string) result

type balance = {
  wallet : string; (* the wallet's label *)
  network : Config.network;
  address : string;
  result : fetch_result;
}

type t = {
  balances : balance list;
  prices : (Config.asset * float) list;
  price_error : string option;
}
