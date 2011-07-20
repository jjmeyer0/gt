{ open Expr_parse;; }

rule token = parse
  | ['\t' ' ' ]+ { token lexbuf }
  | '#'  (_ # ['\n' '\r'])* { token lexbuf }
  | ['\n' '\r']+ as str 
		{ Expr_util.line := 
		  (!Expr_util.line + (String.length str)); 
		  token lexbuf }
  |  ['0'-'9']+  as str { INT((Expr_util.cur_pd(),str)) }
  | "+" { PLUS(Expr_util.cur_pd()) }
  | "*" { TIMES(Expr_util.cur_pd()) }
  | "(" { LPAREN(Expr_util.cur_pd()) }
  | ")" { RPAREN(Expr_util.cur_pd()) }
  | eof { EOF }
  | _ { 
	 failwith((Lexing.lexeme lexbuf) ^ ": lexing error"^
					 (Expr_util.string_of_pos 
						 (Expr_util.cur_pd())))}{}
