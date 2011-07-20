open Grammar;;
open Dump;;

let parsed = 
  let lexbuf = Lexing.from_channel (if(Array.length(Sys.argv) > 1) then (open_in Sys.argv.(1)) else stdin) in
  Parse.grammar Lex.token lexbuf;;

let g = ge2g (gu2ge parsed);;

print_string "This grammar:\n";;
output_grammar stdout g;;
print_string "\n";;

print_string "number of productions is ";;
print_int (num_productions g);;
print_string "\n";;

print_string "the non-terminals are:";;
List.iter (fun s -> print_string " "; output_symbol stdout s) (get_nonterminals g);;
print_string "\n";;

print_string "the terminals are:";;
let terminals = (get_terminals g);;
List.iter (fun s -> print_string " "; output_symbol stdout s) terminals;;

print_string "\nthe start symbol is: ";;
output_symbol stdout (get_start_symbol g);;
print_string "\n\n";;

(* check that we have a lexical class for each terminal *)
match g with
	 Grammar(_,_,_,_,lcs,_,_) ->
		List.iter (fun (t,_) ->
			if (not (List.exists (fun (s,c) -> s = t) lcs)) then
			  (print_string "Error: missing a lexical definition for this terminal: \"";
				print_string t;
				print_string "\"\n\n";
				exit 1)) terminals;;
		
(* check that we do not have duplicate constructors *)
match g with
	 Grammar(_,_,_,ps,_,_,_) ->
		let rec check ps =
		  match ps with
		      [] -> ()
		    | Production(n,s,ss)::ps' ->
		        let (l1,ps') = List.partition (fun (Production(n',s',ss')) -> n' = n) ps' in
		          if (l1 <> []) then
		            (print_string "Error: the term constructor \"";
		             print_string n;
		             print_string "\" is used for two different productions.\n\n";
				     exit 1)
		          else
		            check ps' in
		  check ps;;

dump_lexer g;;
dump_util g;;
dump_Makefile g;;
dump_syntax g;;
dump_parser g;;
dump_eq g;;
dump_prettyprinter g;;
