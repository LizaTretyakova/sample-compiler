type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string

let show_instr instr = match instr with
                       | S_READ ->
                           Printf.eprintf "S_READ\n"
                       | S_WRITE ->
                           Printf.eprintf "S_WRITE\n"
                       | S_PUSH n ->
                           Printf.eprintf "S_PUSH %d\n" n
                       | S_LD x ->
                           Printf.eprintf "S_LD %s\n" x
                       | S_ST x ->
                           Printf.eprintf "S_ST %s\n" x
                       | S_BINOP s ->
                           Printf.eprintf "S_BINOP %s\n" s
                       | _ -> Printf.eprintf "So wow much command! *O*\n"
                       
module Interpreter =
  struct

    let debug_log (state, stack, input, output, code) = 
        Printf.eprintf "\n~~~~~~~~~~~\n[STATE]\n";
        List.iter (fun (name, value) -> Printf.eprintf "(%s %d) " name value) state;
        Printf.eprintf "\n[STACK]\n";
        List.iter (fun value -> Printf.eprintf "%d " value) stack;
        Printf.eprintf "\n[INPUT]\n";
        List.iter (fun value -> Printf.eprintf "%d " value) input;
        Printf.eprintf "\n[OUTPUT]\n";
        List.iter (fun value -> Printf.eprintf "%d " value) output;
        Printf.eprintf "\n[CODE]\n";
        List.iter show_instr code;
        Printf.eprintf "%!"

    let run input code =
      let rec run' (state, stack, input, output) code =
	match code with
	| []       -> output
	| i::code' ->
	    run'
              (match i with
              | S_READ ->
		  let y::input' = input in
		  (state, y::stack, input', output)
              | S_WRITE ->
		  let y::stack' = stack in
		  (state, stack', input, output @ [y])
              | S_PUSH n ->
                  (state, n::stack, input, output)
              | S_LD x ->
		  (state, (List.assoc x state)::stack, input, output)
              | S_ST x ->
		  let y::stack' = stack in
		  ((x, y)::state, stack', input, output)
              | S_BINOP s ->
                  let y::x::stack' = stack in
                  (match s with
                   | "+" | "-" | "*" | "/" | "%" -> 
                         (state, ((Language.s_to_aop s) x y)::stack', input, output)
                   | "<=" | "<" | ">=" | ">" | "==" | "!=" ->
                         (state, (if (Language.s_to_cmpop s x y) then 1 else 0)::stack', input, output)
                   | "&&" | "!!" ->
                         (state, (if (Language.s_to_lop s (x != 0) (y != 0)) then 1 else 0)::stack', input, output)
                   | _ -> failwith "Interpreter: unknown op =^^="
                  )
              )
              code'
      in
      run' ([], [], input, []) code
	
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]

    let rec stmt = function
    | Skip          -> []
    | Assign (x, e) -> expr e @ [S_ST x]
    | Read    x     -> [S_READ; S_ST x]
    | Write   e     -> expr e @ [S_WRITE]
    | Seq    (l, r) -> stmt l @ stmt r

  end
