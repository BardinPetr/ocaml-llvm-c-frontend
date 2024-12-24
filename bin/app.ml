open Core
open Cmdliner

let t_in =
  let doc = "Input file" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"INPUT_FILE")

let t_out =
  let doc = "Output file" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"OUTPUT_FILE")

let app_t = Term.(const C_lang.Main.translate $ t_in $ t_out)
let cmd = Cmd.v (Cmd.info "C LLVM IR fe") app_t
let () = exit (Cmd.eval cmd)
