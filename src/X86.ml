type opnd = R of int | SR of int | S of int | A of int | L of int

let x86regs = [|
  "%ebx"; 
  "%ecx"; 
  "%esi"; 
  "%edi";
  "%eax"; 
  "%edx";
  "%esp";
  "%ebp"
|]

let x86small_regs = [|
        "%al";
        "%ah";
        "%dl";
        "%dh"
|]

let num_of_regs = Array.length x86regs
let word_size = 4

let ebx = R 0
let ecx = R 1
let esi = R 2
let edi = R 3
let eax = R 4
let edx = R 5
let esp = R 6
let ebp = R 7

let al = SR 0
let ah = SR 1
let dl = SR 2
let dh = SR 3

type instr =
| X86Arith of string * opnd * opnd
| X86Idiv of opnd
| X86Cltd
| X86Cmp of opnd * opnd
| X86Xor of opnd * opnd
| X86Mov  of opnd * opnd
| X86Push of opnd
| X86Pop  of opnd
| X86Ret
| X86Set of string * opnd
| X86Call of string
| X86Logic of string * opnd * opnd
| X86Lbl of string
| X86Goto of string
| X86Ifgoto of string * string

module S = Set.Make (String)
module M = Map.Make (String)

class x86env =
  object(self)
    (* val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars *)

    val regs_capacity = num_of_regs-4

    val fname = ref ""
    method get_fname =
        !fname
    method set_fname new_name = 
        fname := new_name
    
    val local_vars = ref M.empty
    method local x = M.find x !local_vars
    val stored_locals = ref 0
    method store_local x = 
        stored_locals := !stored_locals + 1;
        let cur = !stored_locals - 1 in
        local_vars := M.add x (if cur < regs_capacity
                               then (R cur)
                               else (S (cur - regs_capacity))) !local_vars
    method locals = !stored_locals

    val args = ref M.empty
    val stored_args = ref 0
    method store_arg x = 
        stored_args := !stored_args + 1;
        let cur = !stored_args - 1 in
        args := M.add x (A cur) !args
    method args = !stored_args

    val temps = ref 0
    (* method allocate n = temps := max n !temps *)
    method temps = !temps
    method allocated = !temps + !stored_locals

    method allocate stack =
        match stack with
        (* We have stored nothing yet *)
        | [] -> if !stored_locals + 1 <= regs_capacity then R ((!stored_locals + 1) - 1) else S (!stored_locals - regs_capacity)
        (* We have already stored a lot, just next *)
        | (S n)::_ -> S (n + 1)
        (* We have stored yet not that much, probably fit into regs *)
        | (R n)::_ -> if (n + 1) < regs_capacity then R (n + 1) else S 0
        | _ -> failwith "Can't allocae: inconsistent stack\n"

  end

(* let allocate env stack =
  match stack with
  | []                              -> R 0
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs-5 -> R (n+1)
  | _                               -> S 0 *)

module Show =
  struct

    let s_to_arith s = match s with
    | "+" -> "addl"
    | "-" -> "subl"
    | "*" -> "imull"

    let s_to_comp s = match s with
    | "<=" -> "le"
    | "<"  -> "l"
    | ">=" -> "ge"
    | ">"  -> "g"
    | "==" -> "e"
    | "!=" -> "ne"
    | _    -> s (*To be able to pass the suffix explicitly*)

    let s_to_logic s = match s with
    | "&&" -> "andl"
    | "&"  -> "and"
    | "!!" -> "orl"

    let opnd = function
    | R i -> x86regs.(i)
    | SR i -> x86small_regs.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | A i -> Printf.sprintf "%d(%%ebp)" (i * word_size)
    | L i -> Printf.sprintf "$%d" i

    let show_opnd = function
    | R i -> Printf.eprintf "%s\n" x86regs.(i)
    | SR i -> Printf.eprintf "%s\n" x86small_regs.(i)
    | S i -> Printf.eprintf "S %d\n" i
    | A i -> Printf.eprintf "A %d\n" i
    | L i -> Printf.eprintf "L %d\n" i

    let instr = function
    | X86Arith (s, s1, s2) 
                      -> Printf.sprintf "\t%s\t%s,\t%s" (s_to_arith s) (opnd s1) (opnd s2)
    | X86Idiv s       -> Printf.sprintf "\tidiv\t%s" (opnd s)
    | X86Cltd         -> "\tcdq"
    | X86Cmp (s1, s2) -> Printf.sprintf "\tcmpl\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Xor (s1, s2) -> Printf.sprintf "\txorl\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Set (s1, s2) -> Printf.sprintf "\tset%s\t%s" (s_to_comp s1) (opnd s2)
    | X86Mov (s1, s2) -> Printf.sprintf "\tmovl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Push s       -> Printf.sprintf "\tpushl\t%s"      (opnd s )
    | X86Pop  s       -> Printf.sprintf "\tpopl\t%s"       (opnd s )
    | X86Ret          -> "\tret"
    | X86Call p       -> Printf.sprintf "\tcall\t%s" p
    | X86Logic (s, op1, op2)
                      -> Printf.sprintf "\t%s\t%s,\t%s" (s_to_logic s) (opnd op1) (opnd op2)
    | X86Lbl s        -> Printf.sprintf "%s:" s
    | X86Goto s       -> Printf.sprintf "\tjmp\t%s" s
    | X86Ifgoto (cond, s)
                      -> Printf.sprintf "\tj%s\t%s" cond s

                      
  end

module Compile =
  struct

    open StackMachine

    let dest_reg s = match s with
    | "/" -> eax
    | "%" -> edx

    let debug_log stack code = 
        Printf.eprintf "\n~~~~~~~~~~\n[x86 STACK]\n";
        List.iter (fun i -> Printf.eprintf "%s\n" (Show.opnd i)) stack;
        Printf.eprintf "\n[x86 CODE]\n";
        List.iter StackMachine.show_instr code

    let get_fun_name fname = 
        if fname = "main"
        then fname
        else "fun_" ^ fname

    let stack_program env pre_code =
        (* Another env! *)
        (* List.iter (fun i -> show_instr i) pre_code;
        Printf.eprintf "=^^=\n"; *)
        let (_, code) = Interpreter.create_main pre_code in
        List.iter (fun i -> show_instr i) code;
        Printf.eprintf "=^^=\n"; 
        let rec compile stack code env =
            (*debug_log stack code;*)
            (match code with
            | []       -> ([], env)
            | i::code' ->
                show_instr i;
                (* Returns a new environment in case of calling a function. *)
                let (stack', x86code, env') =
                    (match i with
                    | S_READ   -> ([eax], [X86Call "read"], env)
                    | S_WRITE  -> 
                            let s::stack' = stack in
                            ([], [X86Push s; X86Call "write"], env)
                            (* ([], [X86Push (R 0); X86Call "write"; X86Pop (R 0)], env) *)
                    | S_PUSH n ->
                        let s = env#allocate stack in
                        (s::stack, [X86Mov (L n, s)], env)
                    | S_LD x   ->
                        let y = env#local x in
                        let s = env#allocate stack in
                        Printf.eprintf "Load from: ";
                        Show.show_opnd y;
                        Printf.eprintf "Load to: ";
                        Show.show_opnd s;
                        (match (s, y) with
                        | (R _, _) | (_, R _) 
                                     -> (s::stack, [X86Mov (y, s)], env)
                        | _ -> (s::stack, [X86Mov (y, eax);
                                           X86Mov (eax, s)], env))
                    | S_ST x   ->
                        let y = env#local x in
                        let s::stack' = stack in
                        Printf.eprintf "Store to: ";
                        Show.show_opnd y;
                        Printf.eprintf "Store from: ";
                        Show.show_opnd s;
                        (match (s, y) with
                        | (R _, _) | (_, R _) 
                                     -> (stack', [X86Mov (s, y)], env)
                        | _ -> (stack', [X86Mov (s, eax);
                                         X86Mov (eax, y)], env))
                    | S_BINOP s ->
                        let y::x::stack' = stack in
                        (match s with
                         | "+" | "-" | "*" ->
                                (x::stack', [X86Mov (x, eax);
                                             X86Arith (s, y, eax);
                                             X86Mov (eax, x)], env)
                         | "/" | "%" ->
                                (x::stack', [X86Mov (x, eax);
                                             X86Cltd;
                                             X86Idiv y;
                                             X86Mov ((dest_reg s), x)], env)
                         | "<=" | "<" | ">=" | ">" | "==" | "!=" ->
                                (x::stack', [X86Mov (x, edx);
                                             X86Xor (eax, eax);
                                             X86Cmp (y, edx);
                                             X86Set (s, al);
                                             X86Mov (eax, x)], env) 
                         | "&&" ->
                                (x::stack', [X86Mov (y, edx);
                                             X86Xor (eax, eax);
                                             X86Logic (s, edx, edx); (*'op1' has to be register*)
                                             X86Set ("ne", al);
                                             X86Mov (x, edx);
                                             X86Logic (s, edx, edx); (*'op2' has to be register*)
                                             X86Set ("ne", dl);
                                             X86Logic ("&", dl, al);
                                             X86Mov (eax, x)], env)
                         | "!!" ->
                                (x::stack', [X86Xor (eax, eax);
                                             X86Mov (x, edx);
                                             X86Logic (s, y, edx);
                                             X86Set ("ne", al);
                                             X86Mov (eax, x)], env)
                        
                         )
                    | S_LBL s   -> (stack, [X86Lbl s], env)
                    | S_GOTO s  -> (stack, [X86Goto s], env)
                    | S_IFGOTO (cond, s) ->
                        let x::stack' = stack in
                        (stack', [X86Cmp (L 0, x); X86Ifgoto (cond, s)], env)
                    | S_CALL (fname, fargs) -> 
                        let rec push_args stack args = 
                           (match args with
                            | [] -> (stack, [])
                            | arg::args' ->
                                let s::stack' = stack in
                                let (stack'', cmds) = push_args stack' args' in
                                (stack'', [X86Push s] @ cmds)) in
                        let (stack'', cmds) = push_args stack fargs in 
                        let s = env#allocate stack in
                        (s::stack'', [X86Push eax;
                                      X86Push ecx;
                                      X86Push edx]
                                      @ cmds
                                      @ [X86Call (get_fun_name fname);
                                      X86Arith ("+", L ((List.length fargs) * word_size), esp);
                                      X86Mov (eax, s);
                                      X86Pop edx;
                                      X86Pop ecx;
                                      X86Pop eax], (new x86env))
                    | S_BEGIN (fname, fargs, flocals) ->
                        env#set_fname fname;
                        List.iter (fun arg   -> env#store_arg arg) fargs;
                        List.iter (fun local -> env#store_local local) flocals;
                        (stack, [X86Lbl (get_fun_name fname);
                                (* Preserve ebp *)
                                 X86Push ebp;
                                (* Set the new ebp *)
                                 X86Mov (esp, ebp);
                                (* "Allocating" stack for local variables *)
                                 X86Arith ("-", L ((List.length flocals) * word_size), esp)], env)
                    | S_RET ->
                        let s::stack' = stack in
                        (stack', [X86Mov (s, eax)], env)
                    | S_END ->
                                (* Make esp point to the saved ebp of our caller. *)
                        if env#get_fname = "main"
                        then (stack, [X86Mov (ebp, esp);
                                X86Pop ebp;
                                X86Xor (eax, eax);
                                X86Ret], env)
                        else (stack, [X86Mov (ebp, esp);
                                (* Restore the ebp. *)
                                X86Pop ebp;
                                X86Ret], env)
                    | S_DROP ->
                        let s::stack' = stack in
                        (stack', [], env)
                    | _ -> show_instr i; failwith "instr not implemented yet") in
                List.iter (fun s -> Show.show_opnd s) stack'; 
                Printf.eprintf "---\n";
                let (further_code, _) = (compile stack' code' env') in
	            ((x86code @ further_code), env)) (* Return our env, probably modified by LD *)
      in
      compile [] code env (* I hope this is now the env from the arguments. *)

  end

let compile stmt =
  let env = new x86env in
  let (fenv, cmds) = StackMachine.Compile.stmt [] stmt in
  (* let code = Compile.stack_program env @@ cmds in *)
  let (code, env) = Compile.stack_program (new x86env) cmds in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  (* List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars; *)
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
      ),
      (fun () ->
         !"\tmovl\t%ebp,\t%esp";
         !"\tpopl\t%ebp"
      )
  in
  (* !"main:"; *)
  (* prologue(); *)
  List.iter (fun i -> !(Show.instr i)) code;
  (* epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";*)
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  let asm_code = (compile stmt) in
  Printf.fprintf outf "%s" asm_code;
  (*Printf.eprintf "%s" asm_code;*)
  close_out outf;
  match Sys.command (Printf.sprintf "gcc -m32 -g -o %s $RC_RUNTIME/runtime.o %s.s" name name) with
  | 0 -> ()
  | _ -> failwith "gcc failed with non-zero exit code"
