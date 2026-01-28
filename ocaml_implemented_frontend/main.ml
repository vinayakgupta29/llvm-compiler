(* Main entry point for VKS frontend compiler *)

let usage_msg = "vks_frontend <file.vks> [-o output.hir.json]"
let input_file = ref ""
let output_file = ref "output.hir.json"

let speclist = [
  ("-o", Arg.Set_string output_file, "Set output HIR file");
]

let anon_fun filename =
  input_file := filename

let () =
  Arg.parse speclist anon_fun usage_msg;
  
  if !input_file = "" then begin
    Printf.eprintf "Error: No input file specified\n";
    Arg.usage speclist usage_msg;
    exit 1
  end;
  
  (* Read input file *)
  let ic = open_in !input_file in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !input_file };

  try
    (* Parse *)
    Printf.printf "Parsing %s...\n" !input_file;
    let _ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    
    (* Type checking *)
    Printf.printf "Type checking...\n";
    let env = Type_checker.prelude_env in
    
    (* HIR generation *)
    Printf.printf "Generating HIR...\n";
    let hir_program = Hir.ast_to_hir env _ast in
    
    (* Write HIR *)
    Printf.printf "Writing HIR to %s...\n" !output_file;
    Hir_gen.write_hir_to_file !output_file hir_program;
    
    Printf.printf "Frontend compilation complete.\n";
    
  with
  | Lexer.SyntaxError msg ->
      Printf.eprintf "Lexer error: %s\n" msg;
      exit 1
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "%s:%d:%d: Parse error\n"
        !input_file pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
      exit 1
  | Type_checker.TypeError msg ->
      Printf.eprintf "Type error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
