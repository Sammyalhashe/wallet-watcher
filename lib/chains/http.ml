(* HTTP and JSON-RPC transport with timeouts; every call yields a [result]. *)

open Lwt.Infix
open Json

let default_timeout_s = 20.0
let user_agent = "wallet-watcher/0.2"

let headers =
  Cohttp.Header.of_list
    [ ("content-type", "application/json"); ("user-agent", user_agent) ]

let with_basic_auth ~user ~password =
  Cohttp.Header.add_authorization headers (`Basic (user, password))

let with_timeout ?(timeout_s = default_timeout_s) label thread =
  Lwt.pick
    [ thread; (Lwt_unix.sleep timeout_s >|= fun () -> Error (label ^ ": timed out")) ]

let error_message label code body =
  (* Some gateways answer errors with a full HTML page. *)
  let snippet = String.sub body 0 (min 120 (String.length body)) in
  Printf.sprintf "%s: HTTP %d %s" label code (single_line snippet)

let read_response label (resp, body) =
  Cohttp_lwt.Body.to_string body >|= fun body ->
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
  if code >= 200 && code < 300 then Ok body else Error (error_message label code body)

let guard label request =
  Lwt.catch
    request
    (fun exn -> Lwt.return (Error (Printf.sprintf "%s: %s" label (Printexc.to_string exn))))

let get ?(headers = headers) ?timeout_s ?(label = "GET") url =
  with_timeout ?timeout_s label
  @@ guard label (fun () ->
       Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url)
       >>= read_response label)

let post_json ?(headers = headers) ?timeout_s ?(label = "POST") url payload =
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string payload) in
  with_timeout ?timeout_s label
  @@ guard label (fun () ->
       Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url)
       >>= read_response label)

let fetch_json ~label thread extract =
  thread >|= fun response ->
  let* body = response in
  let* json = parse_json body in
  extract json |> Result.map_error (fun e -> Printf.sprintf "%s: %s" label e)

let rpc_result json =
  match member "error" json with
  | Some err when err <> `Null ->
    let msg =
      match path [ "error"; "message" ] json with
      | Some (`String m) -> m
      | _ -> Yojson.Basic.to_string err
    in
    Error ("rpc error: " ^ msg)
  | _ -> ( match member "result" json with
    | Some r -> Ok r
    | None -> missing [ "result" ])

let rpc_payload ~meth ~params =
  `Assoc
    [ ("jsonrpc", `String "2.0"); ("id", `Int 1); ("method", `String meth);
      ("params", `List params) ]

let rpc_call ~label ~url ~meth ~params = post_json ~label url (rpc_payload ~meth ~params)

let fetch_single ~asset ~label fetch parse =
  fetch_json ~label fetch parse
  >|= Result.map (fun amount -> [ Snapshot.holding asset amount ])
