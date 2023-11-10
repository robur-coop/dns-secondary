(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

module K = struct
  open Cmdliner

  let key =
    Arg.conv ~docv:"HOST:HASH:DATA" Dns.Dnskey.(name_key_of_string, pp_name_key)

  let keys =
    let doc = Arg.info ~doc:"nsupdate keys (name:type:value,...)" [ "keys" ] in
    Arg.(value & opt_all key [] doc) |> Mirage_runtime.key

  let name =
    let doc = Arg.info ~doc:"Name of the unikernel" [ "name" ] in
    Arg.(value & opt string "a.ns.robur.coop" doc)

  let ip =
    Arg.conv ~docv:"IP" (Ipaddr.of_string, Ipaddr.pp)

  let monitor =
    let doc = Arg.info ~doc:"monitor host IP" [ "monitor" ] in
    Arg.(value & opt (some ip) None doc)

  let syslog =
    let doc = Arg.info ~doc:"syslog host IP" [ "syslog" ] in
    Arg.(value & opt (some ip) None doc)

end

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Tcpip.Stack.V4V6) = struct
  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _ s =
    let t =
      Dns_server.Secondary.create ~rng:R.generate
        ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign (K.keys ())
    in
    D.secondary s t ;
    S.listen s
end
