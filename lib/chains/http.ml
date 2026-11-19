(* HTTP and JSON-RPC transport with timeouts; every call yields a [result]. *)

open Lwt.Infix
open Json

let timeout_s = 20.0
let user_agent = "portfolio-tui/0.1"

let headers =
  Cohttp.Header.of_list
    [ ("content-type", "application/json"); ("user-agent", user_agent) ]

let with_timeout label thread =
  Lwt.pick
    [ thread; (Lwt_unix.sleep timeout_s >|= fun () -> Error (label ^ ": timed out")) ]

let read_response label (resp, body) =
  Cohttp_lwt.Body.to_string body >|= fun body ->
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
  if code >= 200 && code < 300 then Ok body
  else
    (* Some gateways answer errors with a full HTML page. *)
    let snippet = String.sub body 0 (min 120 (String.length body)) in
    Error (Printf.sprintf "%s: HTTP %d %s" label code snippet)

let guard label thread =
  Lwt.catch
    (fun () -> thread)
    (fun exn -> Lwt.return (Error (Printf.sprintf "%s: %s" label (Printexc.to_string exn))))

let get ?(label = "GET") url =
  with_timeout label
  @@ guard label
       (Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url)
       >>= read_response label)

let post_json ?(label = "POST") url payload =
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string payload) in
  with_timeout label
  @@ guard label
       (Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url)
       >>= read_response label)

let fetch_json ~label thread extract =
  thread >|= fun response ->
  let* body = response in
  let* json = parse_json body in
  extract json |> Result.map_error (fun e -> Printf.sprintf "%s: %s" label e)

let rpc_result json =
  match member "error" json with
  | Some err ->
    let msg =
      match path [ "error"; "message" ] json with
      | Some (`String m) -> m
      | _ -> Yojson.Basic.to_string err
    in
    Error ("rpc error: " ^ msg)
  | None -> ( match member "result" json with
    | Some r -> Ok r
    | None -> missing [ "result" ])

let rpc_call ~label ~url ~meth ~params =
  let payload =
    `Assoc
      [ ("jsonrpc", `String "2.0"); ("id", `Int 1); ("method", `String meth);
        ("params", `List params) ]
  in
  post_json ~label url payload

let fetch_single ~asset ~label fetch parse =
  fetch_json ~label fetch parse
  >|= Result.map (fun amount -> [ (asset, amount) ])
