(* Bittensor: TAO balances from the Taostats API (needs TAOSTATS_API_KEY). *)

open Lwt.Infix
open Json

let api_url = "https://api.taostats.io/api/v3/account/"

let parse_balance json =
  (* The v3 payload shape has varied between releases; [rao] is TAO * 10^9. *)
  let candidates =
    [ [ "data"; "0"; "balance_total" ]; [ "data"; "0"; "balance_free" ];
      [ "balance_total" ]; [ "balance_free" ]; [ "balance" ] ]
  in
  let lookup keys =
    let step acc k =
      match acc with
      | None -> None
      | Some (`List items) -> (
        match int_of_string_opt k with
        | Some i -> List.nth_opt items i
        | None -> None)
      | Some json -> member k json
    in
    List.fold_left step (Some json) keys
  in
  match
    List.find_map
      (fun keys -> match lookup keys with Some v -> as_number v | None -> None)
      candidates
  with
  | Some rao -> Ok (rao /. 1e9)
  | None -> Error "could not locate a balance field in the response"

(* Without an API key the row is reported unavailable, not guessed. *)
let fetch address =
  let label = "TAO " ^ address in
  match Sys.getenv_opt "TAOSTATS_API_KEY" with
  | None | Some "" ->
    Lwt.return (Error "TAO: set TAOSTATS_API_KEY to enable Bittensor balances")
  | Some key ->
    let headers =
      Cohttp.Header.of_list
        [ ("accept", "application/json"); ("authorization", key);
          ("user-agent", Http.user_agent) ]
    in
    let request =
      Http.guard label (fun () ->
        Cohttp_lwt_unix.Client.get ~headers
          (Uri.of_string (api_url ^ Uri.pct_encode address))
        >>= Http.read_response label)
      |> Http.with_timeout label
    in
    Http.fetch_single ~asset:Config.TAO ~label request parse_balance
