module Expr =
  struct

    open Language.Expr

    let rec eval state = function
    | Const  n -> n
    | Var    x -> state x
    | Binop (op, x, y) -> 
        let xv = eval state x in
        let yv = eval state y in
        match op with
        | "+"  -> xv + yv
        | "-"  -> xv - yv
        | "*"  -> xv * yv
        | "/"  -> xv / yv
        | "%"  -> xv mod yv
        | "<=" -> if xv <= yv then 1 else 0
        | "<"  -> if xv < yv then 1 else 0
        | "==" -> if xv == yv then 1 else 0
        | "!=" -> if xv != yv then 1 else 0
        | ">=" -> if xv >= yv then 1 else 0
        | ">"  -> if xv > yv then 1 else 0
        | "&&" -> if (xv != 0) && (yv != 0) then 1 else 0
        | "!!" -> if (xv != 0) || (yv != 0) then 1 else 0
 
  end
  
module Stmt =
  struct

    open StackMachine.Interpreter
    open Language.Stmt

    let eval input stmt =
      let rec eval' ((state, input, output) as c) stmt =
	let state' x = List.assoc x state in
	match stmt with
	| Skip          -> c
	| Seq    (l, r) -> eval' (eval' c l) r
	| Assign (x, e) -> ((x, Expr.eval state' e) :: state, input, output)
	| Write   e     -> (state, input, output @ [Expr.eval state' e])
	| Read    x     ->
	    let y::input' = input in
	    ((x, y) :: state, input', output)
        | If (e, s1, s2)-> eval' c (if ((Expr.eval state' e) == 1) then s1 else s2)
        | While (cond, e, s)  -> if ((cond_to_op cond) (Expr.eval state' e) 0) 
            then (eval' (eval' c s) stmt)
            else c
      in
      let (_, _, result) = eval' ([], input, []) stmt in
      result

  end
