(* Lwt event loop driving the terminal. *)

open Lwt.Infix
open Wallet_watcher

let run (p : Portfolio.t) =
  let term = Notty_lwt.Term.create () in
  let state = ref Nav.initial in
  let draw () = Notty_lwt.Term.image term (Screen.render !state p (Notty_lwt.Term.size term)) in
  let events = Notty_lwt.Term.events term in
  let rec loop () =
    Lwt_stream.get events >>= function
    | None -> Lwt.return_unit
    (* Notty does not redraw on resize; re-render against the new size. *)
    | Some (`Resize _) -> draw () >>= loop
    | Some input -> (
      match Nav.event_of_input input with
      | None -> loop ()
      | Some event -> (
        match Nav.step p !state event with
        | None -> Lwt.return_unit
        | Some next -> state := next; draw () >>= loop))
  in
  draw () >>= loop >>= fun () -> Notty_lwt.Term.release term
