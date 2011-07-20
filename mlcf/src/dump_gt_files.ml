(* This will allow us to add the new files to the main file.
	id will be 0 if we want to call the functions and 1 if
	we would like to actually open the functions in the file. *)
let get_funcs_info_for_main os id args = 
	List.iter( fun s -> 
		if(id=1) then(
			os "open Dump_mlcf_";
			os s; os ";;\n";
		)
		else if(id=0) then (
			os "  dump_mlcf_"; os s; os " g;\n";
		);
	) args;;

(* Dump the new main file for gt *)
let dump_main args =
	let o = open_out("dump_gt_main.ml") in
	let os s = output_string o s in

	os "open Grammar;;\n";
	os "open Dump;;\n";
	os "open Dump_gviz;;\n";
	os "open Dump_syntax_mode;;\n";
	get_funcs_info_for_main os 1 args;

	os "

let v = Version.version 
let vers = (\"Grammar Tool - version \"^v)
let usage = Usage.usage";

	os
	  "

(* check that we have a lexical class for each terminal *)
let checklex (g:grammar) (terminals:symbol list) : unit =
  match g with
		Grammar(_,_,_,_,lcs,_,_) ->
		  List.iter (fun (t,_) ->
			 if (not (List.exists (fun (s,c) -> s = t) lcs)) then (
					 let err = 
						\"Error: missing a lexical definition for this terminal: \\\"\"
						^t^\"\\\"\\n\\n\"
					 in
					 failwith err
				  )

				) terminals;;

(* check that we do not have duplicate constructors *)
let checkconst (g:grammar) : unit = 
  match g with
		Grammar(_,_,_,ps,_,_,_) ->
		  let rec check ps =
			 match ps with
				  [] -> ()
				| Production(n,s,ss,ssop)::ps' ->
				  let (l1,ps') = List.partition (fun (Production(n',s',ss',ssop')) -> n' = n) ps' in
				  if (l1 <> []) then (
					 let err = 
						\"Error: the term constructor \\\"\"^n
						^\"\\\" is used for two different productions.\\n\\n\" 
					 in
					 failwith err
				  )
				  else check ps' 
		  in
		  check ps;;

let longest (li:'a list) : int =
  let rec longest li len =
	 match li with
		  [] -> len
		| hd::tl -> 
		  let slen = String.length (fst hd) in
		  if slen > len then longest tl slen
		  else longest tl len
  in
  longest li 0
;;

let space (len:int) : string =
  let rec space len sp =
	 if len = 0 then sp^\"   \"
	 else space (len-1) (sp^\" \")
  in space len \"\"
;;

let print os (li:'a list) (longest:int) (c:int) : unit = 
  let rec print li i =
	 match li with
		  [] -> ()
		| hd::tl -> 
		  if(i mod c == 0) then os \"\\n\";

		  output_symbol os hd;
		  let dif = longest-String.length (fst hd) in
		  os (space dif);
		  print tl (i+1)
  in print li 0 
;;


type args = string * string;; (* out file * grammar file *)

let sysargs : args =
  let rec getargs (pos:int) (args:args) : args =
	 let a = Sys.argv.(pos) in
	 if a = \"-o\" then (
		Sys.argv.(pos+1),Sys.argv.(Array.length Sys.argv - 1)
	 )
	 else if pos = Array.length Sys.argv - 1 then fst args,Sys.argv.(pos)
	 else getargs (pos+1) args
  in getargs 0 (\"\",\"\")
;;
  


let () =
  (* If -v, -version, -help, --help or -usage are 
	  flagged print usage and exit. *)
  Array.iter(fun a ->
	 if a = \"-version\" || a = \"-v\" then (
		print_string vers; print_string \"\\n\"; exit 1
	 )
	 else if a = \"-help\" || a = \"--help\" || a = \"-usage\" then (
		print_string usage; print_string \"\\n\"; exit 1
	 )
  ) Sys.argv;

  let gname = 
	 if snd sysargs <> \"\" then snd sysargs
	 else \"<stdin>\"
  in
  
  let parsed =
	 let cin =
		if snd sysargs <> \"\" then
		  open_in (snd sysargs)
		else stdin
	 in
	 let lexbuf = Lexing.from_channel cin in
	 Parse.grammar Lex.token lexbuf
  in
  
  let os = 
	 if (fst sysargs) <> \"\" then 
		output_string (open_out (fst sysargs))
	 else print_string
  in

 
  let g = ge2g (gu2ge parsed) in
  os (\"\\n\\nGrammar for file \"^gname^\":\\n\\n\");
  output_grammar os g;


  os \"\\n\\n\\nNumber of Productions: \";
  os (string_of_int  (num_productions g));

  
  os \"\\n\\n\\nNon-terminals:\\n\";
  let nonterminals = get_nonterminals g in
  let length = longest nonterminals in
  print os nonterminals length 2;


  os \"\\n\\n\\nTerminals:\\n\";
  let terminals = (get_terminals g) in
  let length = longest terminals in
  print os terminals length 5;


  os \"\\n\\n\\nStart Symbol: \";
  output_symbol os (get_start_symbol g);
  os \"\\n\\n\";


  checklex g terminals;
  checkconst g;
  dump_lexer g;
  dump_util g;
  dump_main g;
  dump_Makefile g;
  dump_syntax g;
  dump_parser g;
  dump_gviz g;
  dump_syntax_mode g;\n";

	get_funcs_info_for_main os 0 args;;








(* Dump the new make file for gt. 
	Adds the new files that are created
	from the mlcf files. *)
let dump_gt_Make args =
	let o = open_out("dump_gt_Make") in
	let os s = output_string o s in

	(* Add the new files to the make file. This allows us to
		actually use the new mlcf functions w/ gt. *)
	os "gt: trie.cmi grammar.cmi grammar.cmo usage.cmo version.cmo parse.cmo lex.cmo dump.cmi dump.cmo dump_gviz.cmi dump_gviz.cmo dump_syntax_mode.cmo "; 
	List.iter(fun s -> os ("dump_mlcf_"^s^".cmo ")) args; os "main.cmo Makefile\n\t";
	os "ocamlc -o gt util.cmo usage.cmo trie.cmo version.cmo grammar.cmo parse.cmo lex.cmo dump.cmo dump_gviz.cmo dump_syntax_mode.cmo "; 
	List.iter(fun s -> os ("dump_mlcf_"^s^".cmo ")) args; os "main.cmo\n";

os "
emitted:
\t$(MAKE) -f most_recent_emitted_Makefile

main.cmo: main.ml trie.cmi dump_gviz.cmi usage.cmo version.cmo parse.cmi lex.cmo grammar.cmi grammar.cmo dump_syntax_mode.cmo \
    dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo dump_gviz.cmo dump.cmo
\tocamlc -c main.ml

usage.cmo: usage.ml
\tocamlc -c usage.ml

version.cmo: version.ml
\tocamlc -c version.ml

util.cmo: util.ml
\tocamlc -c util.ml

dump.cmi: dump.mli trie.cmo grammar.cmo
\tocamlc -c dump.mli

dump.cmo: dump.ml trie.cmo grammar.cmo
\tocamlc -c dump.ml

dump_gviz.cmi: dump_gviz.mli trie.cmo grammar.cmo
\tocamlc -c dump_gviz.mli

dump_gviz.cmo: dump_gviz.ml trie.cmo grammar.cmo
\tocamlc -c dump_gviz.ml

";

	(* For each of the new functions add commands that
		will allow the user to compile the new code more easily. *)
	List.iter( fun s -> 
		let nm = ("dump_mlcf_"^s) in
		os (nm^".cmo: "^nm^".ml trie.cmo grammar.cmo\n");
		os ("\tocamlc -c "^nm^".ml\n\n");
	) args;

os
"dump_syntax_mode.cmo: dump_syntax_mode.ml trie.cmo grammar.cmo
\tocamlc -c dump_syntax_mode.ml

grammar.cmi: grammar.mli trie.cmo
\tocamlc -c grammar.mli

grammar.cmo: grammar.ml trie.cmo
\tocamlc -c grammar.ml

trie.cmi: trie.mli
\tocamlc -c trie.mli

trie.cmo: trie.ml
\tocamlc -c trie.ml

lex.cmo: lex.ml util.cmo parse.cmi
\tocamlc -c lex.ml

parse.cmo: parse.ml util.cmo grammar.cmo parse.cmi
\tocamlc -c parse.ml

parse.cmi: parse.mli grammar.cmo
\tocamlc -c parse.mli

parse.mli parse.ml: grammar.cmo util.cmo parse.mly
\tocamlyacc -v parse.mly

lex.ml: lex.mll parse.cmi util.cmo
\tocamllex lex.mll

.depend:
\trm -f .depend
\tocamldep *.ml* > .depend

clean:
\trm -f .depend gt lex.ml parse.mli parse.output parse.ml *.cm[iox]

include .depend
"


