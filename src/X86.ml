type opnd = R of int | SR of int | S of int | M of string | L of int

let x86regs = [|
  "%ebx"; 
  "%ecx"; 
  "%esi"; 
  "%edi";
  "%eax"; 
  "%edx"
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

module S = Set.Make (String)

class x86env =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end

let allocate env stack =
  match stack with
  | []                              -> R 0
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs-3 -> R (n+1)
  | _                               -> S 0

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
    | M x -> x
    | L i -> Printf.sprintf "$%d" i

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

    let stack_program env code =
      let rec compile stack code =
        (*debug_log stack code;*)
	match code with
	| []       -> []
	| i::code' ->
	    let (stack', x86code) =
              match i with
              | S_READ   -> ([eax], [X86Call "read"])
              | S_WRITE  -> ([], [X86Push (R 0); X86Call "write"; X86Pop (R 0)])
              | S_PUSH n ->
		  let s = allocate env stack in
		  (s::stack, [X86Mov (L n, s)])
              | S_LD x   ->
		  env#local x;
		  let s = allocate env stack in
                  (match s with
                  | S _ -> (s::stack, [X86Mov (M x, eax);
                                       X86Mov (eax, s)])
                  | _   -> (s::stack, [X86Mov (M x, s)]))
              | S_ST x   ->
		  env#local x;
		  let s::stack' = stack in
                  (match s with
                  | S _ -> (stack', [X86Mov (s, eax);
                                     X86Mov (eax, M x)])
                  | _   -> (stack', [X86Mov (s, M x)]))

	      | S_BINOP s ->
                  let y::x::stack' = stack in
                  (match s with
                   | "+" | "-" | "*" ->
                          (x::stack', [X86Mov (x, eax);
                                       X86Arith (s, y, eax);
                                       X86Mov (eax, x)])
                   | "/" | "%" ->
                          (x::stack', [X86Mov (x, eax);
                                       X86Cltd;
                                       X86Idiv y;
                                       X86Mov ((dest_reg s), x)])
                   | "<=" | "<" | ">=" | ">" | "==" | "!=" ->
                          (x::stack', [X86Mov (x, edx);
                                       X86Xor (eax, eax);
                                       X86Cmp (y, edx);
                                       X86Set (s, al);
                                       X86Mov (eax, x)]) 
                   | "&&" ->
                          (x::stack', [X86Mov (y, edx);
                                       X86Xor (eax, eax);
                                       X86Logic (s, edx, edx); (*'op1' has to be register*)
                                       X86Set ("ne", al);
                                       X86Mov (x, edx);
                                       X86Logic (s, edx, edx); (*'op2' has to be register*)
                                       X86Set ("ne", dl);
                                       X86Logic ("&", dl, al);
                                       X86Mov (eax, x)])
                   | "!!" ->
                          (x::stack', [X86Xor (eax, eax);
                                       X86Mov (x, edx);
                                       X86Logic (s, y, edx);
                                       X86Set ("ne", al);
                                       X86Mov (eax, x)])
                  )
	    in
	    x86code @ compile stack' code'
      in
      compile [] code

  end

let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ StackMachine.Compile.stmt (ref 0) stmt in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars;
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
  !"main:";
  prologue();
  List.iter (fun i -> !(Show.instr i)) code;
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  let asm_code = (compile stmt) in
  Printf.fprintf outf "%s" asm_code;
  (*Printf.eprintf "%s" asm_code;*)
  close_out outf;
  ignore (Sys.command (Printf.sprintf "gcc -m32 -o %s ../runtime/runtime.o %s.s" name name))
