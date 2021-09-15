let cmds = [
  Cli.generate_term;
  Cli.refresh_term;
  Cli.access_term;
]

let () = 
  Cmdliner.Term.(exit @@ eval_choice Cli.default_cmd cmds);