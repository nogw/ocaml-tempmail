open Cmdliner
open Tempmail

let pp v =
  Printf.printf "%s" v

let generate_term =
  let generate_cmd =
    Arg.(value & pos 0 (some string) None & info []) in
  let doc = "generate a new email address, either with the specified address or randomly create one." in
  Term.(const generate $ generate_cmd),
  Term.info "g" ~doc

let access_term =
  let access_cmd =
    Arg.(value & pos 0 (some string) None & info []) in
  let doc = "access email inbox using ID." in
  Term.(const print_t $ access_cmd),
  Term.info "a" ~doc

let refresh_term =
  let doc = "refresh email inbox using ID." in
  let refresh_cmd =
    Arg.(value & pos 0 (some string) None & info []) in
  Term.(const refresh $ refresh_cmd),
  Term.info "r" ~doc

let default_cmd =
  let doc = "tempmail" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "tempmail" ~doc

let cmds = [
  generate_term;
  refresh_term;
  (* access_term; *)
]

let () = Term.(exit @@ eval_choice default_cmd cmds)