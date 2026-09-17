let default_config = "config.json"

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else default_config in
  match Portfolio_lib.Config.load path with
  | exception Portfolio_lib.Config.Parse_error msg ->
    prerr_endline ("config: " ^ msg);
    exit 1
  | [] ->
    prerr_endline ("config: " ^ path ^ " contains no wallets");
    exit 1
  | wallets ->
    Printf.printf "Fetching %d wallets...%!" (List.length wallets);
    Lwt_main.run
      (let open Lwt.Infix in
       Portfolio_lib.Chain_api.fetch_all wallets >>= fun snapshot ->
       (* Clear the progress line before Notty takes over the terminal. *)
       print_string "\r\027[2K";
       flush stdout;
       Portfolio_lib.Tui.run (Portfolio_lib.Portfolio.of_snapshot snapshot))
