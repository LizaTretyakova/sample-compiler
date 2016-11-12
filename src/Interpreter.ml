module Expr =
  struct

    open Language.Expr

    let rec eval feval state = function
    | Const  n -> n
    | Var    x -> state x
    | Binop (op, x, y) -> 
        let xv = eval feval state x in
        let yv = eval feval state y in
        (match op with
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
        | "!!" -> if (xv != 0) || (yv != 0) then 1 else 0)
    | Call (f, args) ->
        (* let fenv' x = List.assoc x fenv in *)
        (* let (fargs, fbody) = fenv' f in
        let args' = List.map (fun arg -> eval fenv state arg) args in
        let state' = List.map2 (fun ident arg -> (ident, arg)) fargs args' in
        let (_, output) = Stmt.eval (fenv, state') fbody in
        List.hd output *)
        let args' = List.map (fun arg -> eval feval state arg) args in
        feval f args'
    | Return x -> eval feval state x

  end
  
module Stmt =
  struct

    open StackMachine.Interpreter
    open Language.Stmt

    let eval conf input stmt =
      let rec eval' ((conf, input, output) as c) stmt =
        let (fenv, state) = conf in
        let state' x = List.assoc x state in
        let feval f args = 
            let fenv' x = List.assoc x fenv in
            let (fargs, fbody) = fenv' f in
            let state'' = List.map2 (fun ident arg -> (ident, arg)) fargs args in
            let (_, res) = eval' ((fenv, state''), input, output) fbody in
            List.hd res 
        in
        match stmt with
	| Skip          -> c
	| Seq    (l, r) -> eval' (eval' c l) r
	| Assign (x, e) -> ((fenv, (x, Expr.eval feval state' e) :: state), input, output)
	| Write   e     -> (conf, input, output @ [Expr.eval feval state' e])
	| Read    x     ->
	    let y::input' = input in
	    ((fenv, (x, y) :: state), input', output)
        | If (e, s1, s2)-> eval' c (if ((Expr.eval feval state' e) == 1) then s1 else s2)
        | While (cond, e, s)  -> if ((cond_to_op cond) (Expr.eval feval state' e) 0) 
            then (eval' (eval' c s) stmt)
            else c
        | Fun (fname, fargs, fbody) -> (((fname, (fargs, fbody)) :: fenv, state), input, output)
      in
      let (result_conf, _, result) = eval' (conf, input, []) stmt in
      (result_conf, result)

  end
