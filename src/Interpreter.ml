module Expr =
  struct

    open Language.Expr

    let rec eval feval state = function
    | Const  n -> (n, [])
    | Var    x -> (state x, [])
    | Binop (op, x, y) -> 
        let (xv, xout) = eval feval state x in
        let (yv, yout) = eval feval state y in
        ((match op with
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
         | "!!" -> if (xv != 0) || (yv != 0) then 1 else 0), xout @ yout)
    | Call (f, args) ->
        let args' = List.map (fun arg -> eval feval state arg) args in
        let args'' = List.map (fun (a, _) -> a) args' in
        let output = List.fold_left (fun l (_, out) -> l @ out) [] args' in
        let (res, eval_output) = feval f args'' in
        (res, output @ eval_output)

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
            let (_, _, eout) = eval' ((fenv, state''), input, []) fbody in
            (match eout with
             | []   -> failwith "Function should return a value"
             | eout -> let res::rout = List.rev eout in 
                       (res, List.rev rout))
        in
        match stmt with
	| Skip                      -> 
            c
        | Seq (Return e, _)         -> 
            eval' c (Return e)
	| Seq (l, r)                -> 
            eval' (eval' c l) r
	| Assign (x, e)             -> 
            let (eres, eout) = Expr.eval feval state' e in
            ((fenv, (x, eres) :: state), input, output @ eout)
        | Write e                   ->
            let (eres, eout) = Expr.eval feval state' e in
            (conf, input, output @ eout @ [eres])
	| Read x                    ->
	    let y::input' = input in
	    ((fenv, (x, y) :: state), input', output)
        | If (e, s1, s2)            ->
            let (eres, eout) = Expr.eval feval state' e in
            eval' (conf, input, output @ eout) (if eres == 1 then s1 else s2)
        | While (cond, e, s)        ->
            let (eres, eout) = Expr.eval feval state' e in
            let c' = (conf, input, output @ eout) in
            if ((cond_to_op cond) eres 0) 
            then (eval' (eval' c' s) stmt)
            else c'
        | Fun (fname, fargs, fbody) -> 
            (((fname, (fargs, fbody)) :: fenv, state), input, output)
        | Return e                  -> 
            let (eres, eout) = Expr.eval feval state' e in
            (conf, input, output @ eout @ [eres])
      in
      let (result_conf, _, result) = eval' (conf, input, []) stmt in
      (result_conf, result)

  end
