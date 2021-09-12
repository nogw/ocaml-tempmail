open Cmdliner

let pp v =
  Printf.printf "ac %s" v

let generate =
  let generate_cmd =
    Arg.(required & pos 0 (some string) None & info []) in
  let doc = "generate a new email address, either with the specified address or randomly create one." in
  Term.(const pp $ generate_cmd),
  Term.info "--g" ~doc

let access =
  let doc = "access email inbox using ID." in
  let access_cmd =
    Arg.(required & pos 0 (some string) None & info []) in
  Term.(const print_t $ access_cmd),
  Term.info "--a" ~doc

let refresh =
  let doc = "refresh email inbox using ID." in
  let refresh_cmd =
    Arg.(required & pos 0 (some string) None & info []) in
  Term.(const print_t $ refresh_cmd),
  Term.info "--r" ~doc

let default_cmd =
  let doc = "use tempmail in your terminal" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "ocaml-tempmail" ~doc

let cmds = [
  generate;
  refresh;
  access;
]

let () = Term.(exit @@ eval_choice default_cmd cmds)