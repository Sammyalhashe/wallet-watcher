let failures = ref 0
let passes = ref 0

let report name ok detail =
  if ok then (incr passes; Printf.printf "  ok   %s\n" name)
  else begin
    incr failures;
    Printf.printf "  FAIL %s: %s\n" name detail
  end

let check name ok = report name ok "condition was false"

(* Balances are floats, so compare with a relative tolerance. *)
let close ?(eps = 1e-9) a b =
  let scale = Float.max 1.0 (Float.max (Float.abs a) (Float.abs b)) in
  Float.abs (a -. b) <= eps *. scale

let check_float name ~expect actual =
  report name (close expect actual)
    (Printf.sprintf "expected %.10g, got %.10g" expect actual)

let check_ok_float name ~expect = function
  | Ok v -> check_float name ~expect v
  | Error e -> report name false ("unexpected Error: " ^ e)

let check_error name = function
  | Error _ -> report name true ""
  | Ok _ -> report name false "expected Error, got Ok"

let check_string name ~expect actual =
  report name (String.equal expect actual)
    (Printf.sprintf "expected %S, got %S" expect actual)

let json = Yojson.Basic.from_string

let summary () =
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1
