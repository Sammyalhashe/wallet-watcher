let fetch_all (_ : Config.wallet list) : Snapshot.t Lwt.t =
  Lwt.return { Snapshot.balances = []; prices = []; price_error = None }
