type i =
| S_READ
| S_WRITE
| S_PUSH   of int
| S_LD     of string
| S_ST     of string
| S_BINOP  of string
| S_LBL    of string
| S_GOTO   of string
| S_IFGOTO of string * string (* z/nz, label *)

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

    let condz  = "z"
    let condnz = "nz"

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

    let find_ip label code = 
        let rec find' label' code' ip =
            match code' with
            | [] -> ip (* invalid value to signal the end of the program *)
            | _  ->
                let i::code'' = code' in
                (match i with
                | S_LBL label' -> ip
                | _            -> find' label' code'' (ip + 1))
        in find' label code 0

    let cond_to_op cond =
        match cond with
        | condz  -> (==)
        | condnz -> (!=)
        | _    -> failwith "unknown cond"
       
    let run input code =
        let rec run' (state, stack, input, output, ip) code =
            if ip >= (List.length code) 
            then output
            else match code with
	    | [] -> output
	    | _  ->
                let i = (List.nth code ip) in
	        run'
                    (match i with
                    | S_READ ->
		        let y::input' = input in
		        (state, y::stack, input', output, ip + 1)
                    | S_WRITE ->
		        let y::stack' = stack in
		        (state, stack', input, output @ [y], ip + 1)
                    | S_PUSH n ->
                        (state, n::stack, input, output, ip + 1)
                    | S_LD x ->
		        (state, (List.assoc x state)::stack, input, output, ip + 1)
                    | S_ST x ->
		        let y::stack' = stack in
		        ((x, y)::state, stack', input, output, ip + 1)
                    | S_BINOP s ->
                        let y::x::stack' = stack in
                        (match s with
                        | "+" | "-" | "*" | "/" | "%" -> 
                            (state, ((Language.s_to_aop s) x y)::stack', input, output, ip + 1)
                        | "<=" | "<" | ">=" | ">" | "==" | "!=" ->
                            (state, (if (Language.s_to_cmpop s x y) then 1 else 0)::stack', input, output, ip + 1)
                        | "&&" | "!!" ->
                            (state, (if (Language.s_to_lop s (x != 0) (y != 0)) then 1 else 0)::stack', input, output, ip + 1)
                        | _ -> failwith "Interpreter: unknown op =^^=")
                    | S_LBL _ ->
                        (state, stack, input, output, ip + 1)
                    | S_GOTO label ->
                        (state, stack, input, output, (find_ip label code))
                    | S_IFGOTO (cond, label) ->
                        let x::stack' = stack in
                        (state, stack', input, output, (if ((cond_to_op cond) x 0) then (find_ip label code) else (ip + 1)))
                    )
                code
      in
      run' ([], [], input, [], 0) code
	
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]

    let rec stmt cnt = function
        (*let cnt = 0 in*) 
        (*match arg with*)
        | Skip           -> []
        | Assign (x, e)  -> expr e @ [S_ST x]
        | Read    x      -> [S_READ; S_ST x]
        | Write   e      -> expr e @ [S_WRITE]
        | Seq    (l, r)  -> stmt cnt l @ stmt cnt r
        | If (e, s1, s2) ->
            cnt := cnt + 3;
            let cnt1 = cnt - 3 in
            let cnt2 = cnt - 2 in
            let cnt3 = cnt - 1 in
            expr e 
            @ [S_IFGOTO (condz, (string_of_int cnt2)); S_LBL (string_of_int cnt1)] 
            @ stmt cnt s1
            @ [S_GOTO (string_of_int cnt3); S_LBL (string_of_int cnt2)] 
            @ stmt cnt s2
            @ [S_LBL (string_of_int cnt3)]
        | While (e, s)   ->
            cnt := cnt + 2;
            let cnt1 = cnt - 2 in
            let cnt2 = cnt - 1 in
            [S_GOTO (string_of_int cnt2); S_LBL (string_of_int cnt1)] 
            @ stmt cnt s 
            @ [S_LBL (string_of_int cnt2)]
            @ expr e 
            @ [S_IFGOTO (condnz, (string_of_int cnt1))]

  end
