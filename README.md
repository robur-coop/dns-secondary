## Secondary DNS server

This is a MirageOS unikernel which is a secondary DNS server on port 53
(TCP and UDP). The data to be served is received via an authenticated zone
transfer, where the IPv4 address of the primary is embedded into the DNSKEY
name.

This can be used with [dns-primary-git](https://github.com/robur-coop/dns-primary-git),
and [let's encrypt](https://github.com/robur-coop/dns-letsencrypt-secondary) for
automated provisioning of let's encrypt certificates.

## Installation from source

To install this unikernel from source, you need to have
[opam](https://opam.ocaml.org) (>= 2.1.0) and
[ocaml](https://ocaml.org) (>= 4.08.0) installed. Also,
[mirage](https://mirageos.org) is required (>= 4.5.0). Please follow the
[installation instructions](https://mirageos.org/wiki/install).

The following steps will clone this git repository and compile the unikernel:

```bash
$ git clone https://github.com/robur-coop/dns-secondary.git
$ mirage configure -t <your-favourite-target>
$ make depend
$ make build
```

## Installing as binary

Reproducible binaries are available at https://builds.robur.coop/job/dns-secondary/

## Questions?

Please open an issue if you have questions, feature requests, or comments.
