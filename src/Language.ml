open Ostap 
open Matcher

let s_to_aop s = 
    match s with
    | "+"  -> (+)
    | "-"  -> (-)
    | "*"  -> ( * )
    | "/"  -> (/)
    | "%"  -> (mod)
    | _    -> failwith "Language.s_to_aop: unknown op =^^="
let s_to_cmpop s =
    match s with
    | "<=" -> (<=)
    | "<"  -> (<)
    | ">=" -> (>=)
    | ">"  -> (>)
    | "==" -> (==)
    | "!=" -> (!=)
    | _    -> failwith "Language.s_to_cmpop: unknown op =^^="
let s_to_lop s =
    match s with
    | "&&" -> (&&)
    | "!!" -> (||)
    | _    -> Printf.eprintf "%s " s; failwith "Language.s_to_lop: unknown op =^^="

module Expr =
  struct

    type t =
    | Const  of int
    | Var    of string
    | Binop  of string * t * t
    | Call   of string * t list
    | Return of (* the Jedi *) t

    ostap (
      parse: ori;

      ori:
        l:andi suf:("!!" andi)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | andi;

      andi:
        l:cmpi suf:("&&" cmpi)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | cmpi;

      cmpi:
        l:addi suf:(("<=" | "<" | "==" | "!=" | ">=" | ">") addi)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | addi;

      addi:
        l:mulli suf:(("+" | "-") mulli)* {
          List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | mulli;

      mulli:
        l:primary suf:(("*" | "/" | "%") primary)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | primary;
      
      primary:
        n:DECIMAL {Const n}
      | f:IDENT -"(" -")" {Call (f, [])}
      | f:IDENT -"(" arg:parse args:(-"," parse)* -")" {Call (f, arg::args)}
      | x:IDENT   {Var   x}
      | %"return" e:parse -";" {Return e}
      | -"(" parse -")"
    )

  end

module Stmt =
  struct

    type t =
    | Skip
    | Read   of string
    | Write  of Expr.t
    | Assign of string * Expr.t
    | Seq    of t * t
    | If     of Expr.t * t * t
    | While  of string * Expr.t * t
    | Repeat of t * Expr.t
    | Fun    of string * string list * t

    let condz  = "z"
    let condnz = "nz"

    ostap (
      parse: s:simple d:(-";" parse)? {
	match d with None -> s | Some d -> Seq (s, d)
      };
      simple:
        x:IDENT ":=" e:!(Expr.parse)     {Assign (x, e)}
      | %"read"  "(" x:IDENT ")"         {Read x}
      | %"write" "(" e:!(Expr.parse) ")" {Write e}
      | %"skip"                          {Skip}
      | %"if" e:!(Expr.parse) "then" s1:!(parse) "else" s2:!(parse) "fi" {If (e, s1, s2)}
      | %"if" e:!(Expr.parse) "then" s1:!(parse) "fi" {If (e, s1, Skip)}
      | %"while" e:!(Expr.parse) "do" s:!(parse) "od" {While (condnz, e, s)}
      | %"repeat" s:!(parse) "until" e:!(Expr.parse) {Seq (s, While (condz, e, s))}
      | %"for" s1:!(parse) "," e:!(Expr.parse) "," s2:!(parse) "do" s:!(parse) "od" {Seq(s1, While (condnz, e, Seq (s, s2)))}
      | %"fun" f:IDENT "(" arg:IDENT? args:(-"," IDENT)* ")" "begin" body:!(parse) "end" 
            {Fun (f, 
                  (match arg with 
                     None     -> (match args with [] -> [] | _ -> failwith "Invalid function declaration: missed argument.\n")
                   | Some arg -> arg::args), 
                  body)}
    )

  end
