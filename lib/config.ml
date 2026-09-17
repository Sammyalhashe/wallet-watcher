(* Parsing of config.json: a mapping of chain -> list of wallets. *)

type chain = ETH | SOL | BTC | TAO | SUI

type wallet = {
  chain : chain;
  label : string;
  address : string;
}

exception Parse_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

let chain_of_string s =
  match String.uppercase_ascii s with
  | "ETH" -> ETH
  | "SOL" -> SOL
  | "BTC" -> BTC
  | "TAO" -> TAO
  | "SUI" -> SUI
  | other -> fail "unknown chain %S (expected ETH, SOL, BTC, TAO or SUI)" other

let string_of_chain = function
  | ETH -> "ETH"
  | SOL -> "SOL"
  | BTC -> "BTC"
  | TAO -> "TAO"
  | SUI -> "SUI"

(* CoinGecko's asset ids, used to look up spot prices. *)
let coingecko_id = function
  | ETH -> "ethereum"
  | SOL -> "solana"
  | BTC -> "bitcoin"
  | TAO -> "bittensor"
  | SUI -> "sui"

let wallet_of_json chain json =
  match json with
  | `Assoc fields ->
    let field name =
      match List.assoc_opt name fields with
      | Some (`String s) -> s
      | Some _ -> fail "%s: field %S must be a string" (string_of_chain chain) name
      | None -> fail "%s: wallet is missing field %S" (string_of_chain chain) name
    in
    { chain; label = field "label"; address = field "address" }
  | _ -> fail "%s: each wallet must be an object" (string_of_chain chain)

(* Returns a flat wallet list; [chain] is carried on each entry so that the
   fetch stage can stay a single concurrent map over one list. *)
let of_json json =
  match json with
  | `Assoc entries ->
    List.concat_map
      (fun (chain_name, wallets) ->
         let chain = chain_of_string chain_name in
         match wallets with
         | `List items -> List.map (wallet_of_json chain) items
         | _ -> fail "%s: value must be an array of wallets" chain_name)
      entries
  | _ -> fail "config must be a JSON object mapping chain -> wallets"

let load path =
  match Yojson.Basic.from_file path with
  | json -> of_json json
  | exception Yojson.Json_error msg -> fail "%s: invalid JSON: %s" path msg
  | exception Sys_error msg -> fail "%s" msg
