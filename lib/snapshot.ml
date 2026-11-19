(* The result of one fetch round, before any analytics. *)

(* What one address holds on one network, e.g. [(AVAX, 12.3); (ETH, 0.4)], or why it failed. *)
type fetch_result = ((Config.asset * float) list, string) result

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
