open Ostap

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident 
         ["read"; "write"; "skip"; "while"; "do"; "od"; "for"; "repeat"; "until"; "if"; "then"; "else"; "fi"; "asm"] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )

    (ostap (!(Language.Stmt.parse) -EOF))


exception Wrong_arguments    
let main =
  try
    let mode, filename =
      match Sys.argv.(1) with
      | "-s" -> `SM , Sys.argv.(2)
      | "-o" -> `X86, Sys.argv.(2)
      | "-i" -> `Int, Sys.argv.(2)
      | _ -> raise Wrong_arguments
    in
    match parse filename with
    | `Ok stmt -> 
	(match mode with
	 | `X86 ->
             let basename = Filename.chop_suffix filename ".expr" in 
	     X86.build stmt basename
	 | _ ->
	     let rec read acc =
	       try
		 let r = read_int () in
		 Printf.printf "> ";
		 read (acc @ [r]) 
               with End_of_file -> acc
	     in
	     let input = read [] in
             let ((fenv, state), output) =
	       match mode with
               | `SM -> let (fenv', cmds) = (StackMachine.Compile.stmt [] stmt) in 
                    (([], []), StackMachine.Interpreter.run input cmds)
               | _   -> Interpreter.Stmt.eval ([], []) input stmt
	     in
             List.iter (fun i -> Printf.printf "%d\n" i) output
	)

    | `Fail er -> Printf.eprintf "%s\n" er
  with 
  | Wrong_arguments ->
      Printf.printf "Usage: rc.byte <command> <name.expr>\n";
      Printf.printf "  <command> should be one of: -i, -s, -o\n"
