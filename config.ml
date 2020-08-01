(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Mirage

let keys =
  let doc = Key.Arg.info ~doc:"nsupdate keys (name:type:value,...)" ["keys"] in
  Key.(create "keys" Arg.(opt (list string) [] doc))

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt (some ip_address) None doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt (some ip_address) None doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "sn.nqsb.io" doc))

let dns_handler =
  let packages =
    [
      package "logs" ;
      package ~min:"5.0.0" ~sublibs:["mirage"] "dns-server";
      package "dns-tsig";
      package "mirage-monitoring";
      package ~sublibs:["mirage"] ~min:"0.3.0" "logs-syslog";
    ]
  and keys = [
    Key.v keys ;
    Key.v name ; Key.v monitor ; Key.v syslog
  ]
  in
  foreign
    ~keys
    ~packages
    "Unikernel.Main" (console @-> random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> stackv4v6 @-> job)

let management_stack = generic_stackv4v6 ~group:"management" (netif ~group:"management" "management")

let () =
  register "secondary" [dns_handler $ default_console $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ generic_stackv4v6 default_network $ management_stack ]
