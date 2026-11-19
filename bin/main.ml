let () =
  let path = ref "" in
  match
    path :=
      (if Array.length Sys.argv > 1 then Sys.argv.(1)
       else Wallet_watcher.Config.default_path ());
    Wallet_watcher.Config.load !path
  with
  | exception Wallet_watcher.Config.Parse_error msg ->
    prerr_endline ("config: " ^ msg);
    exit 1
  | [] ->
    prerr_endline ("config: " ^ !path ^ " contains no wallets");
    exit 1
  | wallets ->
    Printf.printf "Fetching %d wallets...%!" (List.length wallets);
    Lwt_main.run
      (let open Lwt.Infix in
       Wallet_watcher.Chain_api.fetch_all wallets >>= fun snapshot ->
       (* Clear the progress line before Notty takes over the terminal. *)
       print_string "\r\027[2K";
       flush stdout;
       Wallet_watcher_tui.Tui.run (Wallet_watcher.Portfolio.of_snapshot snapshot))
