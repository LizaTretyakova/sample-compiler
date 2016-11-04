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

let show_instr = function
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
    | S_LBL s -> 
        Printf.eprintf "S_LBL %s\n" s
    | S_GOTO s ->
        Printf.eprintf "S_GOTO %s\n" s
    | S_IFGOTO (s1, s2) ->
        Printf.eprintf "S_IFGOTO %s %s\n" s1 s2    
    | _ -> Printf.eprintf "So wow much command! *O*\n"
                       
module Interpreter =
  struct

    let condz  = "z"
    let condnz = "nz"

    let debug_log code = 
        Printf.eprintf "\n[CODE]\n";
        List.iter show_instr code;
        Printf.eprintf "%!"

    let cond_to_op = function
        (* I can just guess that comparing to 'cond[n]z' variables failed because of the = and == issues..*)
        | "z"  -> (==)
        | "nz" -> (!=)
        | _    -> failwith "unknown cond"
       
    let run input code =
        let rec run' (state, stack, input, output, ip) code =
            if ip >= (List.length code) 
            then output
            else match code with
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
                        let v = (if (Language.s_to_lop s (x != 0) (y != 0)) then 1 else 0) in
                        (match s with
                        | "+" | "-" | "*" | "/" | "%" -> 
                            (state, ((Language.s_to_aop s) x y)::stack', input, output, ip + 1)
                        | "<=" | "<" | ">=" | ">" | "==" | "!=" ->
                            (state, (if (Language.s_to_cmpop s x y) then 1 else 0)::stack', input, output, ip + 1)
                        | "&&" | "!!" ->
                            (state, v::stack', input, output, ip + 1)
                        | _ -> failwith "Interpreter: unknown op =^^=")
                    | S_LBL _ ->
                        (state, stack, input, output, ip + 1)
                    | S_GOTO label ->
                        (state, stack, input, output, (List.find ((=) (S_LBL label)) code))
                    | S_IFGOTO (cond, label) ->
                        let x::stack' = stack in
                        (state, stack', input, output, (if ((cond_to_op cond) x 0) then (List.find ((=) (S_LBL label)) code) else (ip + 1)))
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

    let cnt = ref -1 in
    let new_label () =
        cnt := !cnt + 1;
        string_of_int !cnt

    let rec stmt = function
        | Skip           -> []
        | Assign (x, e)  -> expr e @ [S_ST x]
        | Read    x      -> [S_READ; S_ST x]
        | Write   e      -> expr e @ [S_WRITE]
        | Seq    (l, r)  -> stmt cnt l @ stmt cnt r
        | If (e, s1, s2) ->
            (*Printf.eprintf "Compiler: if\n%!";*)
            (*cnt := !cnt + 3;*)
            let lbl1 = new_label in
            let lbl2 = new_label in
            let lbl3 = new_label in
            expr e 
            @ [S_IFGOTO (Interpreter.condz, lbl2); S_LBL lbl1] 
            @ stmt s1
            @ [S_GOTO lbl3; S_LBL lbl2] 
            @ stmt s2
            @ [S_LBL lbl3]
        | While (e, s)   ->
            (*cnt := !cnt + 2;*)
            let lbl1 = new_label in
            let lbl2 = new_label in
            [S_GOTO lbl2; S_LBL lbl1] 
            @ stmt s 
            @ [S_LBL lbl2]
            @ expr e 
            @ [S_IFGOTO (Interpreter.condnz, lbl1)]

  end
