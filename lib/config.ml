(* Parsing of config.json: a list of wallets, each holding one address per
   network. A network is where a balance is read; an asset is what is held. *)

type network = Bitcoin | Ethereum | Base | Avalanche | Solana | Sui | Zcash | Bittensor
type asset = BTC | ETH | AVAX | SOL | SUI | ZEC | TAO

type wallet = {
  label : string;
  addresses : (network * string) list;
}

exception Parse_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

let all_networks = [ Bitcoin; Ethereum; Base; Avalanche; Solana; Sui; Zcash; Bittensor ]

let string_of_network = function
  | Bitcoin -> "bitcoin"
  | Ethereum -> "ethereum"
  | Base -> "base"
  | Avalanche -> "avalanche"
  | Solana -> "solana"
  | Sui -> "sui"
  | Zcash -> "zcash"
  | Bittensor -> "bittensor"

let network_of_string s =
  let key = String.lowercase_ascii s in
  match List.find_opt (fun n -> string_of_network n = key) all_networks with
  | Some n -> n
  | None ->
    fail "unknown network %S (expected one of: %s)" s
      (String.concat ", " (List.map string_of_network all_networks))

let string_of_asset = function
  | BTC -> "BTC"
  | ETH -> "ETH"
  | AVAX -> "AVAX"
  | SOL -> "SOL"
  | SUI -> "SUI"
  | ZEC -> "ZEC"
  | TAO -> "TAO"

let coingecko_id = function
  | BTC -> "bitcoin"
  | ETH -> "ethereum"
  | AVAX -> "avalanche-2"
  | SOL -> "solana"
  | SUI -> "sui"
  | ZEC -> "zcash"
  | TAO -> "bittensor"

let wallet_of_json = function
  | `Assoc fields ->
    let label =
      match List.assoc_opt "label" fields with
      | Some (`String s) -> s
      | Some _ -> fail "wallet field \"label\" must be a string"
      | None -> fail "wallet is missing field \"label\""
    in
    let addresses =
      match List.assoc_opt "addresses" fields with
      | Some (`Assoc entries) ->
        List.map
          (fun (network, address) ->
             match address with
             | `String a -> (network_of_string network, a)
             | _ -> fail "%s: address for %S must be a string" label network)
          entries
      | Some _ -> fail "%s: \"addresses\" must be an object of network -> address" label
      | None -> fail "%s: wallet is missing field \"addresses\"" label
    in
    if addresses = [] then fail "%s: wallet has no addresses" label;
    { label; addresses }
  | _ -> fail "each wallet must be an object"

let of_json = function
  | `Assoc fields -> (
    match List.assoc_opt "wallets" fields with
    | Some (`List items) ->
      let wallets = List.map wallet_of_json items in
      let labels = List.map (fun w -> w.label) wallets in
      List.iter
        (fun l ->
           if List.length (List.filter (String.equal l) labels) > 1 then
             fail "duplicate wallet label %S" l)
        labels;
      wallets
    | Some _ -> fail "\"wallets\" must be an array"
    | None -> fail "config is missing the \"wallets\" array")
  | _ -> fail "config must be a JSON object with a \"wallets\" array"

let load path =
  match Yojson.Basic.from_file path with
  | json -> of_json json
  | exception Yojson.Json_error msg -> fail "%s: invalid JSON: %s" path msg
  | exception Sys_error msg -> fail "%s" msg

(* argv path, then ./config.json, then the XDG location. *)
let config_path_of ~home ~xdg ~local_exists =
  let local = "config.json" in
  let under base = Filename.concat base (Filename.concat "wallet-watcher" "config.json") in
  if local_exists then Ok local
  else
    match (xdg, home) with
    | Some d, _ when d <> "" && not (Filename.is_relative d) -> Ok (under d)
    | _, Some h when h <> "" -> Ok (under (Filename.concat h ".config"))
    | _ -> Error "cannot locate config: set HOME or XDG_CONFIG_HOME"

let default_path () =
  match
    config_path_of ~home:(Sys.getenv_opt "HOME") ~xdg:(Sys.getenv_opt "XDG_CONFIG_HOME")
      ~local_exists:(Sys.file_exists "config.json")
  with
  | Ok path -> path
  | Error msg -> raise (Parse_error msg)
