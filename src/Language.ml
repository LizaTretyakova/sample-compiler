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
    | _    -> failwith "Language.s_to_lop: unknown op =^^="

module Expr =
  struct

    type t =
    | Const of int
    | Var   of string
    | Binop of string * t * t

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
      | x:IDENT   {Var   x}
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
    | While  of Expr.t * t
    | Repeat of t * Expr.t

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
      | %"while" e:!(Expr.parse) "do" s:!(parse) "od" {While (e, s)}
      | %"repeat" s:!(parse) "until" e:!(Expr.parse) {Seq (s, While (e, s))}
    )

  end
