(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
module Main (C : Mirage_console.S) (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Tcpip.Stack.V4V6) (Management : Tcpip.Stack.V4V6) = struct
  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  module Monitoring = Mirage_monitoring.Make(T)(P)(Management)
  module Syslog = Logs_syslog_mirage.Udp(C)(P)(Management)

  let start c _rng _pclock _mclock _ s management =
    let hostname = Key_gen.name () in
    (match Key_gen.syslog () with
     | None -> Logs.warn (fun m -> m "no syslog specified, dumping on stdout")
     | Some ip -> Logs.set_reporter (Syslog.create c management ip ~hostname ()));
    (match Key_gen.monitor () with
     | None -> Logs.warn (fun m -> m "no monitor specified, not outputting statistics")
     | Some ip -> Monitoring.create ~hostname ip management);
    let keys = List.fold_left (fun acc str ->
        match Dns.Dnskey.name_key_of_string str with
        | Error (`Msg msg) -> Logs.err (fun m -> m "key parse error %s" msg) ; exit 64
        | Ok (name, key) -> (name, key) :: acc)
        [] (Key_gen.keys ())
    in
    let t =
      Dns_server.Secondary.create ~rng:R.generate
        ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign keys
    in
    D.secondary s t ;
    S.listen s
end
