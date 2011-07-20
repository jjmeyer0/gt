%{open Expr_syntax;;
  let parse_error s =
	 let error = 
		s^(Expr_util.string_of_pos 
			  (Expr_util.cur_pd())) 
	 in 
	 failwith error;; %}

%start main
%token EOF
%token <Expr_syntax.__term_not_in_ast__> LPAREN PLUS RPAREN TIMES
%token <Expr_syntax.__terminal__> INT
%type <Expr_syntax.expr option> main
%type <Expr_syntax.expr> expr
%type <Expr_syntax.factor> factor
%type <Expr_syntax.mult_term_factor4> mult_term_factor4
%type <Expr_syntax.plus_expr_plus0> plus_expr_plus0
%type <Expr_syntax.term> term
%type <Expr_util.pd> cur_position
%%

main:
  | expr { Some($1) }
  | EOF { None }
	 
cur_position:
  | { Expr_util.cur_pd() }

expr:
  | term plus_expr_plus0 { Plus(pd_term $1, $1, $2) }

factor:
  | INT { Int(get_terminal_pd $1, $1) }
  | LPAREN expr RPAREN { Expr(get_term_pd_not_in_ast $1, $1, $2, $3) }

term:
  | mult_term_factor4 factor { Mult(pd_mult_term_factor4 $1, $1, $2) }

mult_term_factor4:
  | cur_position { ($1, []) }
  | mult_term_factor4 factor TIMES { (pd_mult_term_factor4 $1, 
												  List.rev (($2, $3)::(List.rev (snd $1)))) }

plus_expr_plus0:
  | cur_position { ($1, []) }
  | PLUS term plus_expr_plus0 { (get_term_pd_not_in_ast $1, ($1, $2)::(snd $3)) }
