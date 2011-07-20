let cur_line = ref 1;;
let del = ref 0;;

let rec dummy () = ()
and gviz_terminal (os:string->unit) 
	 (to_pretty_print:bool) (cons:string) = function
 | (d,str1) -> 
	print_new_line os to_pretty_print (fst d);
	let st = ref "" in
	let str1 =
	  String.iter(fun s -> 
		 if(s = '"') then 
		  st := ((!st)^"\\"^(Char.escaped s))
		 else 
		  st := ((!st)^(Char.escaped s))
	  ) str1; !st 
	in 
	os str1
and gviz_expr (os:string->unit) 
	 (to_pretty_print:bool) (cons:string) = function
 | Expr(d,pd1,pd2,expr3) -> 
	let del' = !del in incr del;
	let _ = del' in os "Expr";
	os (string_of_int del'); os ";\n"; os"Expr";
	os (string_of_int del'); os "[label=\``"; os "Expr"; 
	os "\``];\n"; os "Expr"; os (string_of_int del'); 
	os " -- "; os "Expr"; os (string_of_int del'); 
	os (string_of_int del'); os "_med0;\n";os "Expr"; 
	os (string_of_int del'); os (string_of_int del'); 
	os "_med0[label=\``"; os "x"; os "\``];\n"; os "Expr";
	os (string_of_int del'); os "[label=\``"; os "Expr";
	os "\``];\n"; os"Expr"; os (string_of_int del'); 
	os " -- "; os "Expr"; os (string_of_int del');
	os (string_of_int del'); os "_med1;\n"; os "Expr"; 
	os (string_of_int del'); os (string_of_int del'); 
	os "_med1[label=\``"; os "+"; os "\``];\n"; 
	os "Expr"; os (string_of_int del'); os "[label=\``"; 
	os "Expr"; os "\``];\n"; os "Expr"; 
	os (string_of_int del'); os " -- "; 
	gviz_expr os to_pretty_print (cons) expr3; ()
 | Id(d,pd1) ->
	let del' = !del in incr del; 
	let _ = del' in os "Id"; 
	os (string_of_int del'); os ";\n"; os "Id"; 
	os (string_of_int del'); os "[label=\``"; os "Id"; 
	os "\``];\n"; os "Id"; os (string_of_int del'); 
	os " -- "; os "Id"; os (string_of_int del'); 
	os (string_of_int del'); os "_med0;\n"; os "Id";
	os (string_of_int del'); os (string_of_int del'); 
	os "_med0[label=\``"; os "x"; os "\``];\n"; ();;

let gviz (os:string->unit) 
	 (to_pretty_print:bool) (cons:string) e = 
  os ("graph hopeful { \nnode"^
		 " [shape=\``plaintext\``];\n"); 
  gviz_expr os to_pretty_print cons e; os "}";;
