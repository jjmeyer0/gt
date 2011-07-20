open Arith_util;;

type __terminal__ = (pd * string);;
type __term_not_in_ast__ = pd;;

type dummy = Dummy 
and element = 
  | Element0 of pd * __terminal__ 
  | Element1 of pd * __terminal__ 
  | Element2 of pd * __term_not_in_ast__ * expr * __term_not_in_ast__
and expr = | Term of pd * factor * term_expr_op1
and factor = | Fact of pd * fact_factor_unary_op3 * element
and op = 
  | Op0 of pd * __term_not_in_ast__ 
  | Op1 of pd * __term_not_in_ast__
and unary_op = | UnaryOp of pd * __term_not_in_ast__
and fact_factor_unary_op3 = pd * ( unary_op) option
and term_expr_op1 = pd * ( op * factor) list;;


let rec dummy () = () 
and get_terminal_pd = function
  | (pd,_) -> pd 
and get_term_pd_not_in_ast = function
  | (pd) -> pd 
and pd_element = function 
  | Element0(pd,_) -> pd
  | Element1(pd,_) -> pd
  | Element2(pd,_,_,_) -> pd
and pd_expr = function 
  | Term(pd,_,_) -> pd
and pd_factor = function 
  | Fact(pd,_,_) -> pd
and pd_op = function 
  | Op0(pd,_) -> pd
  | Op1(pd,_) -> pd
and pd_unary_op = function 
  | UnaryOp(pd,_) -> pd
and pd_fact_factor_unary_op3 = function 
  | (pd,Some(_)) -> pd
  | (pd,None) -> pd
and pd_term_expr_op1 = function 
  | (pd,[]) -> pd
  | (pd,(_,_)::___tail___) -> pd;;

let pd e = pd_expr e;;
