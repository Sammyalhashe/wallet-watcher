(* The result of one fetch round, before any analytics. *)

type balance = {
  wallet : string; (* the wallet's label *)
  network : Config.network;
  address : string;
  result : ((Config.asset * float) list, string) result;
}

type t = {
  balances : balance list;
  prices : (Config.asset * float) list;
  price_error : string option;
}
