open Grammar;;
open Trie;;

(* this function returns the specified comment
	delimeter - without the quotes. *)
let rec get_comment_delimiter (s:string) : string =
  let delim = ref "" in
  let i = ref 0 in
  String.iter (fun s' ->
	 if(!i <> 0 && !i <> (String.length s) - 1) then (
		delim := !delim^(Char.escaped s')
	 );
	 incr i
  ) s;
  !delim
;;

let get_ln_comment_delim (lcd:string option) : string =
  match lcd with
		None -> "#"
	 | Some(s)-> get_comment_delimiter s
;;

(* returns true if the string - st - starts
	with the substring - sub - otherwise false.*)
let starts_with (sub:string) (st:string) : bool =
  let sub_ln = String.length sub in
  if(sub_ln > String.length st) then false
  else (String.sub st 0 sub_ln = sub)
;;

let is_list (ctornm:string) : bool = 
  starts_with "List_Left" ctornm ||
	  starts_with "List_Right" ctornm
;;

let is_repetition (ctor_nm:string) : bool =
  starts_with "List_Left_Repetition" ctor_nm ||
	  starts_with "List_Right_Repetition"	ctor_nm
;;

let is_opt (ctornm:string) : bool = starts_with "Option`" ctornm;;

let symbol_var_name ((s,b):string*bool) : string = s;;

let starts_with_nonterminal (ss:symbol list) : bool =
  match ss with
		[] -> false
	 | s::ss' -> (not (is_terminal s))
;;

(* Returns a list of pairs where a pair is (x,n) - 
	x representing the data of the original list and 
	a number n. *)
let numbered (xs:symbol list) : (symbol * int) list =
  let n = ref 0 in
  List.map (fun x ->
	 n := !n+1;
	 (x,!n)
  ) xs
;;

(* Position of the symbol_var_name of s in given list ss *)
let svn_pos (ss:symbol list) (s:string) : int = 
  let n = ref 0 in 
  let n' = ref 0 in
  if(not(starts_with_nonterminal ss)) then (incr n);
  List.iter(fun s' ->
	 incr n;
	 if((symbol_var_name s') = symbol_var_name (s,false)) then (n' := !n);
  ) ss; !n'
;;









let dump_lexer (g:grammar) : unit =
  match g with
   	Grammar(name,_,lcd,_,lcs,t,_) ->
		  let ofile = (open_out (name ^ "_lex.mll")) in
		  let os = output_string ofile in
		  let cap_nm = String.capitalize name in
		  let util_line = cap_nm ^ "_util.line" in
		  os "{\n (* auto-generated by gt *)\n\nopen ";
		  os cap_nm;
		  os "_parse;;\n ";
		  os "\n}\n\n";
		  os "rule token = parse\n";
		  os "| ['\\t' ' ' '\\n' '\\r']+ as str { ";
		  os "let i = ref 0 in String.iter(fun s -> if s <> ' ' && s <> '\\t' then incr i) str; ";
		  os (util_line^" := !"^util_line^" + !i; token lexbuf }\n");

		  let delim = get_ln_comment_delim lcd in
		  os "| ";
		  String.iter(fun c -> os ("'"^(Char.escaped c)^"' ")) delim;
		  os " (_ # ['\\n' '\\r'])* { token lexbuf }\n";
		  
		  List.iter (fun (s,c) -> 
			 let in_ast = trie_contains t s in
			 os "| ";
			 if starts_with "char" c then (
				os (String.sub c 4 ((String.length c)-4))
			 )
			 else os c;
			 if(in_ast) then os " as str";
			 os " { ";
 			 
			 os (symbol_var_name (s,true));
			 if in_ast then (
				if(starts_with "char" c) then
				  os ("("^cap_nm^"_util.cur_pd(),Char.escaped str)")
				else
				  os ("(("^cap_nm^"_util.cur_pd(),str))")
			 )
			 else ( 
				os ("("^cap_nm^"_util.cur_pd())")
			 );
			 os " }\n"
		  ) lcs;
		  
		  os "| eof { EOF }\n";
		  os "| _ {failwith((Lexing.lexeme lexbuf) ^";
		  os "\": lexing error\"^(";
		  os cap_nm; os "_util.string_of_pos (";
		  os cap_nm; os "_util.cur_pd())))}";
		  os "{}\n";
		  close_out ofile
;;










(* This function creates the *_syntax.ml file from
	the grammar given to gt. *)
let dump_syntax (g:grammar) : unit =
  match g with
		Grammar(name,start_symbol,_,ps,_,t,_) ->
		  let ofile = (open_out (name ^ "_syntax.ml")) in
		  let os = output_string ofile in
		  let n = ref "" in
		  os "(* auto-generated by gt *)\n\n";
		  os "open ";
		  os (String.capitalize name);
		  os "_util;;\n\n";
		  os "(* This type is used for terminals.\nWe do this to have better position";
		  os " data.*)\ntype __terminal__ = (pd * string);;\n";
		  os "type __term_not_in_ast__ = pd;;\n";
		  
		  os "type dummy = Dummy ";
		  List.iter (fun (Production(c,s,ss,ssops)) -> 
			 let has_ors = match ssops with None -> false | _ -> true in
			 let i = ref 0 in
			 let prods ss =
				let i' = string_of_int !i in
				incr i;
				let fnm = symbol_var_name (s,false) in 
				if (s <> !n) then (
				  n := s;
				  os "\nand ";
				  os fnm;
				  os " ="
				);
				let n' = ref (svn_pos ss s) in
				let is_list = is_list c in
				let is_opt = is_opt c in
				
				if((List.length ss <> 0 && (is_opt || (!n' <> 0 && is_list))) || 
						(not is_list && not is_opt)) then (
				  if not is_list && not is_opt then (
					 if has_ors then
						os (" | "^c^i'^" of pd")
					 else os (" | "^c^" of pd")
				  )
				  else os " pd * (";
				  
				  let first = ref false in
				  
				  List.iter (fun s' -> 
					 if(not is_list && not is_opt) then os " * "
					 else if((symbol_var_name s')<>symbol_var_name (s,false)) then (
						if !first then os " * " else os " ";
						first := true
					 );
					 
					 if is_terminal s' then (
						if is_in_ast t s' then os "__terminal__"
						else os "__term_not_in_ast__"
					 )
					 else (
						if(not((is_list || is_opt) && (symbol_var_name s') = symbol_var_name (s,false))) then
						  os (symbol_var_name s')
					 )
				  ) ss;
				  
				  if is_opt then os ") option";
				  if is_list then os ") list";
				);
			 in

			 let ssops = match ssops with None -> [] | Some(x) -> x in
			 prods ss;
			 List.iter (prods) ssops;
		  ) ps;
		  os ";;\n\n";
		  
	     (* now dump functions to extract pd *)
	     os "(* pd stands for pos (position) *)\n";
	     os "let rec dummy () = () \n";
		  os "and get_terminal_pd = function\n   | (pd,_) -> pd ";
		  os "\nand get_term_pd_not_in_ast = function\n   | (pd) -> pd ";
		  n := "";
		  List.iter (fun (Production(c,s,ss,ssops)) ->
			 let has_ors = match ssops with None -> false | _ -> true in
			 let i = ref 0 in
			 let prods ss =
				let i' = string_of_int !i in
				incr i;

				if (s <> !n) then (
				  n := s;
				  os "\nand ";
				  os ("pd_" ^ (symbol_var_name (s,false)));
				  os " = function "
				);
				os "\n  | ";
				
				let n' = ref (svn_pos ss s) in
				let is_list = is_list c in
				let is_opt = is_opt c in
				
				if((List.length ss <> 0 && (is_opt || (!n' <> 0 && is_list))) || (not is_list && not is_opt)) then (
				  if(not is_list && not is_opt) then (
					 if(has_ors) then os (c^i')
					 else os c
				  );
				  os "(pd";
				  if(is_opt) then os ",Some(";
				  if(is_list) then os ",(";
				  
				  let fir = ref true in
				  
				  List.iter (fun s' ->
					 if(not is_opt && not is_list) then os (",_")
					 else if((symbol_var_name s') <> symbol_var_name (s,false)) then (
						if(!fir) then (fir := false; os "_")
						else os (",_");
					 )
				  ) ss;
				  
				  
				  
				  if(is_opt || is_list) then os ")";
				  
				  if(is_list) then os "::___tail___";
				  os ") -> pd";
				)
				else (
				  if(is_opt) then (os "(pd,"; os "None"; os ") -> pd");
				  if(is_list) then (os "(pd,"; os "[]"; os ") -> pd");
				);
				
			 in

			 let ssops = match ssops with None -> [] | Some(x) -> x in
			 prods ss;
			 List.iter (prods) ssops;
		  ) ps;
		  
		  os ";;\n";
		  os ("let pd e = pd_" ^ symbol_var_name (get_start_symbol g) ^ " e;;");
		  
		  close_out ofile
;;


let print_parser_list (os:string->unit) (li:'a list) t (in_ast:bool) : unit =
  List.iter (fun s -> 
	 if(is_in_ast t s && in_ast) then begin
		os " "; os (symbol_var_name s)
	 end else if not (is_in_ast t s) && not in_ast then
		os (" "^symbol_var_name s)
  ) li;
;;


let print_parser_error (os:string->unit) (cap_name:string) (syntax:string) : unit =
  os "%{\n(* auto-generated by gt *)\n\n   open ";
  os syntax; os ";;\n";
  os "let parse_error s =\n\t ";
  os "let error = s^(";
  os cap_name; os "_util.string_of_pos (";
  os cap_name; os "_util.cur_pd()))";
  os " in failwith error;;\n\n";
  os "%}\n\n";
;;

let print_parser_nonterminals (os:string->unit) (nonterminals:symbol list) 
	                                     (start_var:string) (util:string) (syntax:string) : unit =
  let os_type_decl n v = os ("\n%type <"^syntax^"."^n^"> "^v) in
  os ("\n\n%type <"^syntax^"."^start_var^" option> main");
  List.iter(fun s -> os_type_decl (symbol_var_name s) (symbol_var_name s)) nonterminals;
;;

let dump_parser (g:grammar) : unit =
  let terminals = get_terminals g in
  let nonterminals = get_nonterminals g in
  match g with
		Grammar(name,start_symbol,_,productions,_,t,_) ->
		  let start_var = (symbol_var_name (start_symbol,false)) in
		  let ofile = (open_out (name ^ "_parse.mly")) in
		  let os = output_string ofile in
		  let cap_name = (String.capitalize name) in
		  let syntax = cap_name^"_syntax" in
		  let util = cap_name^"_util" in

		  let rec is_cur_pos_used productions = 
			 match productions with
				  [] -> false
				| (Production(n,s,ss,ssops))::tl ->
				  if List.length ss = 0 then true
				  else is_cur_pos_used tl
		  in

		  print_parser_error os cap_name syntax;
		  os "%start main\n\n%token EOF\n";
		  os ("%token <"^cap_name^"_syntax.__term_not_in_ast__>");
		  print_parser_list os terminals t false;
		  os ("\n%token <"^cap_name^"_syntax.__terminal__>");
		  print_parser_list os terminals t true;

		  (* Print type declarations of nonterminals *)
		  print_parser_nonterminals os nonterminals start_var util syntax;
		  if(is_cur_pos_used productions) then (
			 os ("\n%type <"^util^".pd> cur_position")
		  );
		  os "\n\n%%\n\n\n\nmain:\n\t| ";
		  os start_var; os " { Some($1) }\n\t| EOF { None }\n\n"; 

		  if(is_cur_pos_used productions) then begin
			 os "\n\ncur_position:\n| { "; os (util^".cur_pd() }\n")
		  end;
		  
		  List.iter(fun p ->
			 match p with
				  Production(n,s,ss,ssops) ->
					 let has_ors = match ssops with None -> false | _ -> true in
					 let i = ref 0 in
					 os ("\n"^(symbol_var_name (s,false))^":");
					 let prods ss = 
						let i' = string_of_int !i in
						incr i;
						os "\n  |";
						let ctor_nm = n in
						let starts_with_nonterminal = starts_with_nonterminal ss in
						let is_list = is_list ctor_nm in
						let is_opt = is_opt ctor_nm in
						let is_repetition = is_repetition ctor_nm in
						
						if List.length ss = 0 then begin
						  os " cur_position"
						end;
						List.iter(fun s' -> os " "; os (symbol_var_name s')) ss;

						os " { ";
						let r = ref 1 in
						let n' = ref (svn_pos ss s) in
						let is_emp = (List.length ss) = 0 in
						if(not is_opt && not is_list) then (
						  if(has_ors) then
							 os (ctor_nm^i')
						  else os ctor_nm
						);
						if (not starts_with_nonterminal) then (
						  if(List.length ss = 0) then os "($1"
						  else (
							 if(not is_emp && is_in_ast t (List.hd ss)) then os "(get_terminal_pd $1"
							 else if(not is_emp) then  os "(get_term_pd_not_in_ast $1"
							 else os "("
						  )
						);
						if(is_opt && List.length ss = 0) then (os ", None");
						if(is_list && List.length ss = 0) then (os ", []");
						
						let first = ref true in
						let first' = ref true in

						List.iter (fun s' -> 
					     if (!first) then (
							 first := false;
				          if (starts_with_nonterminal) then
				            os ("(pd_"^(symbol_var_name s')^" $1");
							 if(is_opt) then os ", Some(";
							 if(is_list) then (
								if(starts_with "List_Left" ctor_nm) then os ",List.rev ((" 
								else os ", (";
							 )
						  );
						  if((not((symbol_var_name s') = symbol_var_name (s,false) && is_list))) then (
							 if(!first' && (is_opt || is_list)) then ( 
								first' := false;
								if(is_opt) then os " "; 
								os "$"; 
								os (string_of_int !r);
								if(is_opt) then os " ";
							 )
							 else (
								if(is_repetition) then os "::$"					
								else os ", $";
								os (string_of_int !r);
							 );
						  ); 
						  incr r
						) ss; 

						if(is_list && List.length ss <> 0) then (
						  if(not starts_with_nonterminal && !n' <> 0) then decr n';
						  
						  if(is_repetition) then (
							 if(!n' <> 0) then os ("::(snd $"^(string_of_int !n')^"))")
							 else os "::[])";			
						  )				
						  else (
							 let is_left = starts_with "List_Left" ctor_nm in
							 if(is_left) then os ")::(List.rev ";

							 if(!n' <> 0) then (
								if not is_left then os ")::";
								os ("(snd $"^(string_of_int !n')^")")
							 )
							 else os ")::[]";

							 if(starts_with "List_Left" ctor_nm) then os "))" 
						  )
						);
 						if(is_opt && List.length ss <> 0) then os ")"; 
						os ") }\n"  
					 in

					 let ssops = match ssops with None -> [] | Some(x) -> x in
					 prods ss;
					 List.iter (prods) ssops;
					 
		  ) productions;
		  close_out ofile
;;

let dump_main (g:grammar) : unit = 
  match g with
		Grammar(name,_,_,_,_,_,_) ->
		  let ofile = open_out (name ^ "_main.ml") in
		  let os = output_string ofile in
		  let cap_name = String.capitalize name in
		  os 
"(* auto-generated by gt *)
let usage = \"./grammar_name [options] <file> 
    The options are:
       -p      Reprints the contents of <file> to
               <file>pp.txt. Without this option
               the contents are printed to the terminal.
       -a      Prints a text view of <file>'s AST to 
               <file>ast.txt.
       -g      Outputs a Graphviz file named <file>gviz.dot
               that contains a visual representation of <file>'s
               syntax tree.
       -help   prints this
\"
let num_args : int = Array.length Sys.argv
let file_name : string = Sys.argv.(num_args - 1)
let ofile (file:string) : (string->unit) = output_string (open_out file);;

";


		  os 
"
let is_flag (arg:string) : bool = 
  let rec is_flag (i:int) : bool = 
	 if i = num_args then false
    else if arg = Sys.argv.(i) then true
	 else is_flag (i + 1)
  in is_flag 0
;;
";


		  os "
let () =
  if is_flag \"-help\" || num_args <= 1 then (print_string usage; exit 1);
  let parsed =
     let lexbuf = Lexing.from_channel (
       if num_args > 1 then (open_in Sys.argv.(num_args - 1)) 
	    else stdin) 
	  in ";

		  os cap_name; os "_parse.main ";
		  os cap_name; os "_lex.token lexbuf \nin\n";
		  os "
  match parsed with
		None -> ()
	 | Some(x) ->
		if is_flag \"-p\" then (
		  let os = ofile (file_name^\"pp.txt\") in
        ";
		  os cap_name; os "_pp.pp os true x;";
		  os "
		  print_string \"finished pretty print\";
		) else ";
		  os cap_name; os "_pp.pp print_string true x;";
		  os "
      print_string \"\\n\";
		if is_flag \"-a\" then (
		  let os = ofile (file_name^\"ast.txt\") in
        ";
		  os cap_name; os "_ppast.ppast os false x;";		  
		  os "
		  print_string \"finished ast\\n\";
		);

		if is_flag \"-g\" then (
		  let os = ofile (file_name^\"gviz.dot\") in
        ";
		  os cap_name; os "_gviz.gviz os false \"\" x;";
		  os "
		  print_string \"finished gviz\\n\"
		); 

		print_string \"done\\n\"

";
		  close_out ofile
;;



let dump_util (g:grammar) : unit = 
  match g with
		Grammar(name,_,_,_,_,_,_) ->
		  let ofile = open_out (name^"_util.ml") in
		  let os = output_string ofile in
		  os "(* auto-generated by gt *)

let fname = ref (
   if Array.length(Sys.argv) > 1 then Sys.argv.(1) 
   else \"<stdin>\"
);;

let line = ref 1;;
type pd = int * string;;
let string_of_pos (p:pd) = \" on line \"^(string_of_int (fst p))^\" in file \"^(snd p);;
let cur_pd():pd = (!line,!fname);; (* \"pd\": pos *) 
";
		  close_out ofile
;;



let dump_Makefile (g:grammar) : unit =
  match g with
		Grammar(name,_,_,_,_,_,_) ->
		  let link s os = 
		    os ("\tocamlc -o "^s^" ");
		    os (List.fold_right (fun x s -> name^"_"^x^".cmo "^s) 
					 ["util"; "syntax"; "parse"; "lex"; "pp"; "ppast"; "gviz"; "eq"; "main"] "\n\n") 
		  in

		  let compile os s = os ("\tocamlc -c "^name^"_"^s^"\n\n") in

		  let link_deps (os:string->unit) (file:string) (ext:string) (deps:string list) : unit =
			 os (name^"_"^file^ext^": "); 
			 List.iter (fun s -> os (name^"_"^s^" ")) deps; os "\n"
		  in

		  let link_gt_funs os =
			 List.iter(fun f -> 
				link_deps os f ".cmo" [(f^".ml"); "syntax.cmo"];
				compile os (f^".ml") 
			 ) [ "pp"; "ppast"; "gviz"; "eq"]
		  in
		  
		  let yacc os = os ("\tocamlyacc -v "^name^"_parse.mly\n\n") in
		  let lex os = os ("\tocamllex "^name^"_lex.mll\n") in
		  
	  	  let ofile = (open_out (name ^ "_Makefile")) in
	  	  let os = output_string ofile in
		  
		  os "# auto-generated by gt \n\n";
		  os ("\n"^name^": "^name^"_util.cmo "^name^"_syntax.cmo "^name^"_parse.cmo ");
		  os (name^"_lex.cmo "^name^"_pp.cmo "^name^"_eq.cmo "^name^"_gviz.cmo "^name^
				  "_ppast.cmo "^name^"_main.cmo\n");
		  
		  link name os;

	     link_deps os "main" ".cmo" ["main.ml"; "ppast.cmo"; "pp.cmo"; "parse.cmo"; "lex.cmo"; "gviz.cmo" ];
		  compile os "main.ml";
		  
		  link_deps os "syntax" ".cmo" ["syntax.ml"; "util.cmo"];
		  compile os "syntax.ml";
		  link_gt_funs os;
		  
		  link_deps os "util" ".cmo" ["util.ml"];
		  compile os "util.ml" ;

		  link_deps os "lex" ".cmo" ["lex.ml"; "util.cmo"; "parse.cmi"];
		  compile os "lex.ml" ;

		  link_deps os "parse" ".cmo" ["parse.ml"; "util.cmo"; "syntax.cmo"; "parse.cmi"];
		  compile os "parse.ml" ;

		  link_deps os "parse" ".cmi" ["parse.mli"; "syntax.cmo"];
		  compile os "parse.mli" ;

		  os (name^"_parse.mli "^name^"_parse.ml: "^name^"_parse.mly "^name^"_syntax.cmo\n");
		  yacc os;
		  
		  link_deps os "lex" ".ml" ["lex.mll"; "parse.cmi"];
		  lex os;

		  os ("\nclean:\n\trm -f "^name^"_lex.ml "^name^"_parse.mli "^name^"_parse.ml *.cmo *.cmi "^name^"\n\n");
		  close_out ofile;
		  
		  let ofile = (open_out "most_recent_emitted_Makefile") in
		  let os = output_string ofile in
		  os "all:\n\t$(MAKE) -f ";
		  os name;
		  os "_Makefile";
		  close_out ofile;
		  
		  (* dump a BAT file for Windows compilation *)		  
	  	  let ofile = (open_out (name ^ "_compile.bat")) in
	  	  let os = output_string ofile in
		  List.iter (compile os) [ "util.ml" ; "syntax.ml" ];
		  yacc os;
		  lex os;
		  let files = 
			 ["parse.mli"; "parse.ml"; "lex.ml"; "pp.ml"; "ppast.ml"; 
			  "gviz.ml"; "syntax_mode.ml"; "eq.ml"; "main.ml"] 
		  in
		  List.iter (compile os) files;
		  link (name^".exe") os
;;