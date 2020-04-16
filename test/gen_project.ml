type contents = Format.formatter -> unit

let dune_get ppf =
  Fmt.pf ppf
  {|((config
  ((root_packages (((name alcotest)) ((name duniverse))))
   (excludes (((name duniverse)))) (pins ()) (pull_mode Source) (remotes ())
   (ocaml_compilers (4.07 4.08 4.09 4.10)) (branch master)))
 (deps
  ((opamverse ())
   (duniverse
    (((dir alcotest.1.1.0) (upstream https://github.com/mirage/alcotest.git)
      (ref ((t 1.1.0) (commit 4974a9b652cbec92b9aaa71354be9a6c85d14f52)))
      (provided_packages (((name alcotest) (version (1.1.0))))))
     ((dir astring.0.8.3+dune)
      (upstream git://github.com/dune-universe/astring.git)
      (ref
       ((t duniverse-v0.8.3)
        (commit 26e04c11f3163c25d942cf191428d62a2c5748fe)))
      (provided_packages (((name astring) (version (0.8.3+dune))))))
     ((dir base.v0.13.1) (upstream https://github.com/janestreet/base.git)
      (ref ((t v0.13.1) (commit 9f1001c0024257d3a63940a47efd11058300bcfa)))
      (provided_packages (((name base) (version (v0.13.1))))))
     ((dir bigarray-compat.1.0.0)
      (upstream https://github.com/mirage/bigarray-compat.git)
      (ref ((t v1.0.0) (commit 1b7e1fe8ed50f940d98a7ef09fed50ad5329f538)))
      (provided_packages (((name bigarray-compat) (version (1.0.0))))))
     ((dir bos.0.2.0+dune) (upstream git://github.com/dune-universe/bos.git)
      (ref
       ((t duniverse-v0.2.0)
        (commit 6c69eddd9a56f3caa21a7240c37cbf1c7b7ba1a1)))
      (provided_packages (((name bos) (version (0.2.0+dune))))))
     ((dir cmdliner.1.0.4+dune)
      (upstream git://github.com/dune-universe/cmdliner.git)
      (ref
       ((t duniverse-v1.0.4)
        (commit d1bd753d88d297018c69a51070a399049dd1f966)))
      (provided_packages (((name cmdliner) (version (1.0.4+dune))))))
     ((dir cppo.1.6.6) (upstream https://github.com/ocaml-community/cppo.git)
      (ref ((t v1.6.6) (commit 51c598bf4a4732fdd9be1b16a5b26743ec4023d2)))
      (provided_packages (((name cppo) (version (1.6.6))))))
     ((dir dune-configurator.2.4.0)
      (upstream https://github.com/ocaml/dune.git)
      (ref ((t 2.4.0) (commit ccd447e41a711f8a52bc854d71dba8677c900c34)))
      (provided_packages
       (((name dune-configurator) (version (2.4.0)))
        ((name dune-private-libs) (version (2.4.0))))))
     ((dir findlib.1.8.1+dune)
      (upstream https://github.com/dune-universe/lib-findlib)
      (ref
       ((t duniverse-1.8.1)
        (commit 55b3374114ea425e8881e4758a06b02ba06aabb5)))
      (provided_packages
       (((name findlib) (version (1.8.1+dune)))
        ((name ocamlfind) (version (1.8.1+dune))))))
     ((dir fmt.0.8.8+dune) (upstream https://github.com/dune-universe/fmt)
      (ref
       ((t duniverse-v0.8.8)
        (commit 342fa6d95e847694b71f798ad55cb932b64d0d35)))
      (provided_packages (((name fmt) (version (0.8.8+dune))))))
     ((dir fpath.0.7.2+dune)
      (upstream git://github.com/dune-universe/fpath.git)
      (ref
       ((t duniverse-v0.7.2)
        (commit 9402e6033c994ab3201be04c6f8e8a101e737ab9)))
      (provided_packages (((name fpath) (version (0.7.2+dune))))))
     ((dir logs.0.7.0+dune)
      (upstream git://github.com/dune-universe/logs.git)
      (ref
       ((t duniverse-v0.7.0)
        (commit 4f141048f71b1d84b8e283086c62ca270aa13051)))
      (provided_packages (((name logs) (version (0.7.0+dune))))))
     ((dir lwt.5.2.0) (upstream https://github.com/ocsigen/lwt.git)
      (ref ((t 5.2.0) (commit ca9861a599657587af6d80fba65443c58b202d0d)))
      (provided_packages (((name lwt) (version (5.2.0))))))
     ((dir mmap.1.1.0) (upstream https://github.com/mirage/mmap.git)
      (ref ((t v1.1.0) (commit 46f613db11c00667764523ccbb3d63e53e1c666c)))
      (provided_packages (((name mmap) (version (1.1.0))))))
     ((dir num.1.3+dune) (upstream git://github.com/dune-universe/num.git)
      (ref
       ((t duniverse-v1.3) (commit bdb2d7653e927e142b701b51d89f393471279713)))
      (provided_packages (((name num) (version (1.3+dune))))))
     ((dir ocaml-compiler-libs.v0.12.1)
      (upstream https://github.com/janestreet/ocaml-compiler-libs.git)
      (ref ((t v0.12.1) (commit c202a46641ed0bb20eac15ea4f005cbd9f3e5c01)))
      (provided_packages (((name ocaml-compiler-libs) (version (v0.12.1))))))
     ((dir ocaml-migrate-parsetree.1.6.0)
      (upstream https://github.com/ocaml-ppx/ocaml-migrate-parsetree.git)
      (ref ((t v1.6.0) (commit 93444dbd6153ec0ef6e9daebef6f1da53927792b)))
      (provided_packages
       (((name ocaml-migrate-parsetree) (version (1.6.0))))))
     ((dir ocaml-version.2.4.0)
      (upstream https://github.com/ocurrent/ocaml-version.git)
      (ref ((t v2.4.0) (commit 1eb91307b47cb46508273d4f63faa95a8173a411)))
      (provided_packages (((name ocaml-version) (version (2.4.0))))))
     ((dir ocamlgraph.1.8.8+dune)
      (upstream git://github.com/dune-universe/ocamlgraph.git)
      (ref
       ((t duniverse-v1.8.8)
        (commit 8b911e0c9c17b6c996b4af8e27fe5edf4e766d81)))
      (provided_packages (((name ocamlgraph) (version (1.8.8+dune))))))
     ((dir ocplib-endian.1.1)
      (upstream https://github.com/OCamlPro/ocplib-endian.git)
      (ref ((t 1.1) (commit 6ee41bb7f8f2bef103bcf4e05ace57597c0d2316)))
      (provided_packages (((name ocplib-endian) (version (1.1))))))
     ((dir opam-core.2.0.6+dune)
      (upstream git://github.com/dune-universe/opam.git)
      (ref
       ((t duniverse-2.0.6)
        (commit 004066ae560ec87d82030a107a7a58cd79b47e98)))
      (provided_packages
       (((name opam-core) (version (2.0.6+dune)))
        ((name opam-format) (version (2.0.6+dune))))))
     ((dir opam-file-format.2.0.0+dune)
      (upstream git://github.com/dune-universe/opam-file-format.git)
      (ref
       ((t duniverse-2.0.0)
        (commit ec0db8081136a93f4d6873dba90e0f0d29d1b43b)))
      (provided_packages (((name opam-file-format) (version (2.0.0+dune))))))
     ((dir parsexp.v0.13.0)
      (upstream https://github.com/janestreet/parsexp.git)
      (ref ((t v0.13.0) (commit e7dc1cb24716740c6dcf5d5d687a37793be8a4aa)))
      (provided_packages (((name parsexp) (version (v0.13.0))))))
     ((dir ppx_derivers.1.2.1)
      (upstream git://github.com/ocaml-ppx/ppx_derivers.git)
      (ref ((t 1.2.1) (commit d6655353f647d33de12d215aaa477ba936febef3)))
      (provided_packages (((name ppx_derivers) (version (1.2.1))))))
     ((dir ppx_sexp_conv.v0.13.0)
      (upstream https://github.com/janestreet/ppx_sexp_conv.git)
      (ref ((t v0.13.0) (commit 03c42d49513564809400de62b4515afaedb094a8)))
      (provided_packages (((name ppx_sexp_conv) (version (v0.13.0))))))
     ((dir ppxlib.0.12.0) (upstream https://github.com/ocaml-ppx/ppxlib.git)
      (ref ((t 0.12.0) (commit f13dc352b9bb17e8ced3d12d2533cffba2fcbfac)))
      (provided_packages (((name ppxlib) (version (0.12.0))))))
     ((dir re.1.9.0) (upstream https://github.com/ocaml/ocaml-re.git)
      (ref ((t 1.9.0) (commit 42c7f8899c9b1a4e011053487a97cf7eab312d5f)))
      (provided_packages (((name re) (version (1.9.0))))))
     ((dir result.1.5) (upstream https://github.com/janestreet/result.git)
      (ref ((t 1.5) (commit b0b9cd6e9cce8d2b7ad11127a6135540ec3f8a92)))
      (provided_packages (((name result) (version (1.5))))))
     ((dir rresult.0.6.0+dune)
      (upstream git://github.com/dune-universe/rresult.git)
      (ref
       ((t duniverse-v0.6.0)
        (commit 1ff30e48943f69f7e7bcac211b56f6c7c76eeccb)))
      (provided_packages (((name rresult) (version (0.6.0+dune))))))
     ((dir seq.base+dune) (upstream https://github.com/c-cube/seq.git)
      (ref ((t 0.2.2) (commit 6934813195285661cba3e017e7d3cd79d362bb2f)))
      (provided_packages (((name seq) (version (base+dune))))))
     ((dir sexplib.v0.13.0+dune)
      (upstream https://github.com/dune-universe/sexplib.git)
      (ref
       ((t duniverse-v0.13)
        (commit c41a3f1831c07323dea41d48dfb999239f336e9e)))
      (provided_packages (((name sexplib) (version (v0.13.0+dune))))))
     ((dir sexplib0.v0.13.0)
      (upstream https://github.com/janestreet/sexplib0.git)
      (ref ((t v0.13.0) (commit 659d33a4a23d86d86114829a05275f70a6dc79fe)))
      (provided_packages (((name sexplib0) (version (v0.13.0))))))
     ((dir stdio.v0.13.0) (upstream https://github.com/janestreet/stdio.git)
      (ref ((t v0.13.0) (commit 5541155e501cc1e7e94df8557b465ddf037ab68b)))
      (provided_packages (((name stdio) (version (v0.13.0))))))
     ((dir stdlib-shims.0.1.0)
      (upstream https://github.com/ocaml/stdlib-shims.git)
      (ref ((t 0.1.0) (commit d32f70e5bec8255903e9b27f89c49da812ad9b6e)))
      (provided_packages (((name stdlib-shims) (version (0.1.0))))))
     ((dir stringext.1.6.0)
      (upstream https://github.com/rgrinberg/stringext.git)
      (ref ((t 1.6.0) (commit 2bce0a6fe54e8f8782f7a3b2be44a5e1fb37a522)))
      (provided_packages (((name stringext) (version (1.6.0))))))
     ((dir uri.3.1.0) (upstream https://github.com/mirage/ocaml-uri.git)
      (ref ((t v3.1.0) (commit 21f76c72f126be44ac29c7cb22e1fc95ef9c8b0c)))
      (provided_packages
       (((name uri) (version (3.1.0))) ((name uri-sexp) (version (3.1.0))))))
     ((dir uuidm.0.9.7+dune)
      (upstream git://github.com/dune-universe/uuidm.git)
      (ref
       ((t duniverse-v0.9.7)
        (commit fa124fd3c0a82f7005fdac982e520c9c066488c4)))
      (provided_packages (((name uuidm) (version (0.9.7+dune))))))))))
 (depexts ()))
|}

let opam ppf =
  Fmt.pf ppf
    {|opam-version: "2.0"
maintainer:   "Camelus Bactrianus"
authors:      ["Camelus Bactrianus"]
license:      "ISC"
homepage:     "https://www.example.com"
bug-reports:  "https://www.example.com/issues"
dev-repo:     "git+https://example.com/repo.git"

build: [
 ["dune" "subst"] {pinned}
 ["dune" "build" "-p" name "-j" jobs]
 ["dune" "runtest" "-p" name] {with-test}
]

depends: [
  "ocaml"   {>= "4.09"}
  "dune"
  "fmt"
  "logs"
  "alcotest" {with-test}
]

synopsis: "Example project generated for testing purposes"
|}

let ocamlformat ~version ppf =
  Fmt.pf ppf {|version = %s
profile = conventional
  |} version

let empty_file _ppf = ()

(* Project generation logic *)

type file = Folder of string * file list | File of string * contents

let print_to_file path printer =
  let channel = open_out path in
  let formatter = Format.formatter_of_out_channel channel in
  printer formatter;
  Format.pp_print_newline formatter ();
  close_out channel

let rec mkdir_p path =
  try Unix.mkdir path 0o777 with
  | Unix.Unix_error (EEXIST, _, _) -> ()
  | Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename.dirname path in
      mkdir_p parent;
      Unix.mkdir path 0o777

let rec instantiate ~root =
  mkdir_p root;
  List.iter (function
    | Folder (name, contents) ->
        mkdir_p name;
        instantiate ~root:(Filename.concat root name) contents
    | File (name, printer) -> print_to_file (Filename.concat root name) printer)
