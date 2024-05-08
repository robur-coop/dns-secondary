(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

module K = struct
  open Cmdliner

  let key =
    Arg.conv ~docv:"HOST:HASH:DATA" Dns.Dnskey.(name_key_of_string, pp_name_key)

  let keys =
    let doc = Arg.info ~doc:"nsupdate keys (name:type:value,...)" ["keys"] in
    Arg.(value & opt_all key [] doc)
end

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK) (M : Mirage_clock.MCLOCK) (T : Mirage_time.S) (S : Tcpip.Stack.V4V6) = struct
  module D = Dns_server_mirage.Make(P)(M)(T)(S)

  let start _rng _pclock _mclock _ s keys =
    let t =
      Dns_server.Secondary.create ~rng:R.generate
        ~tsig_verify:Dns_tsig.verify ~tsig_sign:Dns_tsig.sign keys
    in
    D.secondary s t ;
    S.listen s
end
