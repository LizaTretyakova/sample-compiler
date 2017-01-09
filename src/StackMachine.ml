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
| S_ASM    of string list * string list

let builtins = [|
    "main";
    "read";
    "write"
|]

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
    | S_CALL (fname, _) ->
        Printf.eprintf "S_CALL %s ...\n" fname
    | S_BEGIN (fname, _, _) ->
        Printf.eprintf "S_BEGIN %s ... ...\n" fname
    | S_END ->
        Printf.eprintf "S_END\n"
    | S_RET ->
        Printf.eprintf "S_RET\n"
    | S_DROP ->
        Printf.eprintf "S_DROP\n"
    | S_ASM (cmds, _) ->
        List.iter (fun cmd -> Printf.eprintf "S_ASM %s ...\n" cmd) cmds
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

    let rec find_ip_func fname code =
        match code with
        | [] -> Printf.eprintf "Hasn't found %s\n" fname; failwith "function not found"
        | i::code' -> 
            (match i with
            | S_BEGIN (fname', _, _) -> if (fname' = fname) then 0 else 1 + find_ip_func fname code' 
            | _                     -> 1 + find_ip_func fname code')


    let rec get_locals locals code = 
        match code with
        | [] -> locals
        | i::code' ->
            (match i with
            | S_ST x -> 
                (get_locals
                    (if List.exists ((=) x) locals
                    then locals
                    else x::locals) code')
            | _ -> get_locals locals code')

    let create_main code =
        let ip = List.length code in
        let code' = List.rev code in
        let rec create_main' ip rev_code_begin code_end =
            (match rev_code_begin with
            | [] -> (ip, [S_BEGIN ("main", [], (get_locals [] code_end))]
                         @ code_end
                         @ [S_END])
            | instr::rest_begin ->
                (match instr with
                | S_END -> (ip, (List.rev rev_code_begin)
                                    @ [S_BEGIN ("main", [], (get_locals [] code_end))]
                                    @ code_end
                                    @ [S_END])
                | _     -> (create_main' (ip - 1) rest_begin (instr::code_end))))
        in create_main' ip code' []

    let rec create_state args stack = 
        match args with
        | [] -> ([], stack)
        | arg::args' -> 
            let i::stack' = stack in
            let (args'', stack'') = create_state args' stack' in
            ((arg, i)::args'', stack')

    let rec cut_func ip code output = 
        let cmd = List.nth code ip in
        match cmd with
        | S_END -> output
        | _     -> cut_func (ip + 1) code (output @ [cmd])

    let run input full_code =
        let rec run' ((state, stack, input, output, ip) as c) code =
            if ip >= (List.length code) 
            then c
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
                        (state, stack(*'*), input, output, ip + 1)
                    | S_CALL (fname, fargs) ->
                        if fname = "read"
                        then 
		            let y::input' = input in
		            (state, y::stack, input', output, ip + 1)
                        else if fname = "write"
                        then
		            let y::stack' = stack in
		            (state, stack', input, output @ [y], ip + 1)
                        else
                            let it = find_ip_func fname code in
                            (state, (ip + 1)::stack, input, output, it)
                    | S_BEGIN (fname, fargs, flocals) -> 
                        let ret_addr::stack' = 
                            if fname = "main"
                            then (List.length code - 1)::stack 
                            else stack in
                        let (state', stack'') = create_state fargs stack' in
                        let (_, ret_stack, input', out, _) = run' (state', [], input, [], ip + 1) code in
                        let final_stack = 
                            if fname = "main"
                            then ret_stack
                            else let res::rest = ret_stack in res::stack''
                        in
                        (state, final_stack, input', out @ output, ret_addr)
                    | S_RET -> 
                            (state, stack, input, output, List.length code) (* It's time to finish this >:3 *)
                    | S_END -> (state, stack, input, output, ip + 1)
                    )
                code
      in
      let (iter, new_code) = create_main full_code in
      Printf.eprintf "***\n";
      List.iter (fun i -> show_instr i) new_code;
      Printf.eprintf "***\n";
      let (_, _, _, result, _) = run' ([], [], input, [], iter) new_code in
      result
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr fenv = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> (expr fenv x) @ (expr fenv y) @ [S_BINOP s]
    | Language.Expr.Call (f, args) -> 
        (List.fold_left (fun l arg -> l @ (expr fenv arg)) [] args)
        @ [S_CALL (f, List.assoc f fenv)]

    let cnt = ref (-1) 
    let new_label () =
        cnt := !cnt + 1;
        "label_" ^ (string_of_int !cnt)
    let rec extract_locals locals = function
        | Assign (x, _) -> 
            if List.exists ((==) x) locals
            then locals
            else x::locals
        | Seq (Return e, _) -> locals
        | Seq (l, r) | If (_, l, r) -> extract_locals (extract_locals locals l) r
        | While (_, _, s) -> extract_locals locals s
        | Skip | Write _ | Read _ | Fun (_, _, _) | Return _ | Asm (_, _) -> locals

    let rec stmt fenv = function
        | Skip             -> (fenv, [])
        (*| Assign ("_", e)  -> (fenv, (expr fenv e) @ [S_DROP])*)
        | Language.Stmt.Call (f, args) -> (fenv, (expr fenv (Language.Expr.Call (f, args))) @ [S_DROP])
        | Assign (x, e)    -> (fenv, (expr fenv e) @ [S_ST x])
        | Read    x        -> (fenv, [S_CALL ("read", []); S_ST x])
        | Write   e        -> (fenv, (expr fenv e) @ [S_CALL ("write", [""]); S_DROP])
        | Seq    (l, r)    -> 
            let (fenvl, codel) = stmt fenv l in
            let (fenvr, coder) = stmt fenvl r in
            (fenvr, codel @ coder)
        | If (e, s1, s2)   ->
            let lbl1 = new_label () in
            let lbl2 = new_label () in
            let lbl3 = new_label () in
            let (_, code1) = stmt fenv s1 in (* fenv both times because the branches probably shouldn't affect each other. *)
            let (_, code2) = stmt fenv s2 in (* Omit the returned fenv sunce it shouldn't introduce new functions. *)
            (fenv, (expr fenv e) 
            @ [S_IFGOTO (condz, lbl2); S_LBL lbl1] 
            @ code1
            @ [S_GOTO lbl3; S_LBL lbl2] 
            @ code2
            @ [S_LBL lbl3])
        | While (cond, e, s) ->
            let lbl1 = new_label () in
            let lbl2 = new_label () in
            let (fenv', code') = stmt fenv s in (* fenv' shoud be equivalent to fenv for the same reason as above. *)
            (fenv', [S_GOTO lbl2; S_LBL lbl1] 
            @ code'
            @ [S_LBL lbl2]
            @ (expr fenv e) (* It seems more appropriate to use fenv here, not fenv'. *)
            @ [S_IFGOTO (cond, lbl1)])
        | Fun (fname, fargs, fbody) ->
            let (fenv', code') = stmt fenv fbody in
            ([(fname, fargs)] @ fenv' @ fenv, [S_BEGIN (fname, fargs, (extract_locals [] fbody))] 
            @ code'
            @ [S_END])
        | Return e ->
            (fenv, (expr fenv e) @ [S_RET])
        | Asm (cmds, vars) ->
            (fenv, [S_ASM (cmds, vars)])

  end
