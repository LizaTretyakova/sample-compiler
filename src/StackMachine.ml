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
| S_CALL   of string * string list
| S_BEGIN  of string * string list * string list
| S_END
| S_RET
| S_DROP

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

    let debug_log code = 
        Printf.eprintf "\n[CODE]\n";
        List.iter show_instr code;
        Printf.eprintf "%!"

    let cond_to_op = function
        (* I can just guess that comparing to 'cond[n]z' variables failed because of the = and == issues..*)
        | "z"  -> (==)
        | "nz" -> (!=)
        | _    -> failwith "unknown cond"
       
    let rec find_ip label code =
        match code with
        | [] -> failwith "label not found"
        | i::code' -> if i = S_LBL label then 0 else 1 + find_ip label code'

    let run input code =
        let rec run' (state, stack, input, output, ip) code =
            if ip >= (List.length code) 
            then output
            else let i = (List.nth code ip) in
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
                            let v = (if (Language.s_to_lop s (x != 0) (y != 0)) then 1 else 0) in
                            (state, v::stack', input, output, ip + 1)
                        | _ -> failwith "Interpreter: unknown op =^^=")
                    | S_LBL label ->
                        (*Printf.eprintf "S_LBL %s" label;*)
                        (state, stack, input, output, ip + 1)
                    | S_GOTO label ->
                        (*Printf.eprintf "S_GOTO %s" label;*)
                        (state, stack, input, output, (find_ip label code))
                    | S_IFGOTO (cond, label) ->
                        (*Printf.eprintf "S_IFGOTO %s" label;*)
                        let x::stack' = stack in
                        (state, stack', input, output, (if ((cond_to_op cond) x 0) then (find_ip label code) else (ip + 1)))
                    | S_DROP ->
                        let y::stack' = stack in
                        (state, stack', input, output, ip + 1)
                    | S_CALL (fname, fargs) ->

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
    | Call (f, args) -> (List.map (fun arg -> expr arg) args)

    let cnt = ref (-1) 
    let new_label () =
        cnt := !cnt + 1;
        string_of_int !cnt
    let rec extract_locals locals = function
        | Assign (x, _) -> 
            if List.exists ((==) x) locals
            then locals
            else x::locals
        | Seq (Return e, _) -> locals
        | Seq (l, r) | If (_, l, r) -> extract_locals (extract_locals locals l) r
        | While (_, _, s) -> extract_locals locals s
        | Skip | Write _ | Read _ | Fun (_, _, _) | Return _ -> locals

    let rec stmt fenv = function
        | Skip             -> (fenv, [])
        | Assign (drop, e) -> (fenv, expr e @ [S_DROP])
        | Assign (x, e)    -> (fenv, expr e @ [S_ST x])
        | Read    x        -> (fenv, [S_READ; S_ST x])
        | Write   e        -> (fenv, expr e @ [S_WRITE])
        | Seq    (l, r)    -> (fenv, stmt l @ stmt r)
        | If (e, s1, s2)   ->
            let lbl1 = new_label () in
            let lbl2 = new_label () in
            let lbl3 = new_label () in
            (fenv, expr e 
            @ [S_IFGOTO (condz, lbl2); S_LBL lbl1] 
            @ stmt s1
            @ [S_GOTO lbl3; S_LBL lbl2] 
            @ stmt s2
            @ [S_LBL lbl3])
        | While (cond, e, s) ->
            let lbl1 = new_label () in
            let lbl2 = new_label () in
            (fenv, [S_GOTO lbl2; S_LBL lbl1] 
            @ stmt s 
            @ [S_LBL lbl2]
            @ expr e 
            @ [S_IFGOTO (cond, lbl1)])
        | Fun (fname, fargs, fbody) ->
            ([(fenv, fargs)] @ fenv, [S_BEGIN (fname, fargs, (exptract_locals [] fbody))] 
            @ stmt fbody
            @ [S_END])
        | Return e ->
            (fenv, expr e @ [S_RET])

  end
