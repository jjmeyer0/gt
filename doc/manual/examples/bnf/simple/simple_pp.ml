let cur_line = ref 1;;
let rec print_new_line (os:string->unit) 
	 (do_print:bool) (p:int) : unit =
  if(p > !cur_line && do_print) then ( 
	 os "\n"; 
	 incr cur_line;
	 print_new_line os do_print p;
  )
;;

let rec dummy () = () 
and pp_terminal (os:string->unit) (to_pretty_print:bool) = function 
  | (d,str1) -> 
	 print_new_line os to_pretty_print (fst d); 
	 os str1
and pp_expr (os:string->unit) (to_pretty_print:bool) = function 
  | Expr(d,pd1,pd2,expr3) -> 
	 print_new_line os to_pretty_print (fst d); 
	 print_new_line os to_pretty_print (fst pd1); 
	 os "x"; os " ";
	 print_new_line os to_pretty_print (fst d); 
	 print_new_line os to_pretty_print (fst pd2); 
	 os "+"; os " ";
	 pp_expr os to_pretty_print expr3; () 
  | Id(d,pd1) -> 
	 print_new_line os to_pretty_print (fst d); 
	 print_new_line os to_pretty_print (fst pd1); 
	 os "x"; os " "; ();;

let pp (os:string->unit) (to_pretty_print:bool) e = 
  pp_expr os to_pretty_print e;;
