type opnd = R of int | S of int | M of string | L of int

let x86regs = [|
  "%eax"; 
  "%ebx"; 
  "%ecx"; 
  "%edx"; 
  "%esi"; 
  "%edi"
|]

let num_of_regs = Array.length x86regs
let word_size = 4

let eax = R 0
let ebx = R 1
let ecx = R 2
let edx = R 3
let esi = R 4
let edi = R 5

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
| X86Set of string * string
| X86Call of string

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
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
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

    let opnd = function
    | R i -> x86regs.(i)
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
    | X86Set (s1, s2) -> Printf.sprintf "\tset%s\t%s" (s_to_comp s1) s2
    | X86Mov (s1, s2) -> Printf.sprintf "\tmovl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Push s       -> Printf.sprintf "\tpushl\t%s"      (opnd s )
    | X86Pop  s       -> Printf.sprintf "\tpopl\t%s"       (opnd s )
    | X86Ret          -> "\tret"
    | X86Call p       -> Printf.sprintf "\tcall\t%s" p

                      
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
        debug_log stack code;
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
		  (s::stack, [X86Mov (M x, s)])
              | S_ST x   ->
		  env#local x;
		  let s::stack' = stack in
		  (stack', [X86Mov (s, M x)])
	      | S_BINOP s ->
                  let y::x::stack' = stack in
                  (match s with
                   | "+" | "-" | "*" ->
                          (y::stack', [(* Preserve eax. *)
                                       X86Push eax;
                                       
                                       X86Mov (x, eax);
                                       X86Arith (s, y, eax);
                                       X86Mov (eax, y);
                                       
                                       X86Pop eax])
                   | "/" | "%" ->
                          (y::stack', [X86Push eax;
                                       
                                       X86Mov (x, eax);
                                       
                                       (* Cannot divide by edx. *)
                                       X86Push ebx;
                                       X86Mov (y, ebx);
                                       X86Cltd;
                                       X86Idiv ebx;
                                       X86Pop ebx;

                                       X86Mov ((dest_reg s), y);
                                       
                                       X86Pop eax])
                   | "<=" | "<" | ">=" | ">" | "==" | "!=" ->
                           (y::stack', [X86Push eax;
                                       (* Avoid arguments being 
                                        * stored at eax. 
                                        * What a mess :( *)
                                       X86Push ebx;
                                       X86Push ecx;
                                       X86Mov (x, ebx);
                                       X86Mov (y, ecx);

                                       X86Xor (eax, eax);
                                       X86Cmp (ecx, ebx);
                                       X86Set (s, "%al");
                                       (* It's important to restore 
                                        * them here, because y can 
                                        * accidently be one of them.*)
                                       X86Pop ecx;
                                       X86Pop ebx;
                                       X86Mov (eax, y);

                                       X86Pop eax]) 
                  )
	    in
	    x86code @ compile stack' code'
      in
      compile [] code

  end

let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ StackMachine.Compile.stmt stmt in
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
  Printf.eprintf "%s" asm_code;
  close_out outf;
  ignore (Sys.command (Printf.sprintf "gcc -m32 -o %s ../runtime/runtime.o %s.s" name name))
