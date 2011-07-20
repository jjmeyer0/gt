open Simple_syntax;;

let rec dummy () = () 
and eq_expr = function
  | Expr(d,pd1,pd2,expr3),Expr(d',pd1',pd2',expr3') ->
    true && eq_expr (expr3,expr3')
  | Id(d,pd1),Id(d',pd1') -> true
  | _ -> false;;

let eq e = eq_expr e;;
