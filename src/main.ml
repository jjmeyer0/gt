open Grammar
open Grammare
open Grammaru
open Dump;;
open Dump_gviz;;
open Dump_syntax_mode;;
open Dump_mlcf_ppast;;
open Dump_mlcf_pp;;
open Dump_mlcf_eq;;


let v = Version.version 
let vers = ("Grammar Tool - version "^v)
let usage = Usage.usage

(* check that we have a lexical class for each terminal *)
let checklex (g:Grammar.grammar) (terminals:Grammar.symbol list) : unit =
  match g with
      Grammar.Grammar(_,_,_,_,_,lcs,_,_) ->
	List.iter (fun (t,_) ->
	  if (not (List.exists (fun (s,c) -> s = t) lcs)) then (
	    let err = 
	      "Error: missing a lexical definition for this terminal: \""
	      ^t^"\"\n\n"
	    in
	    failwith err
	  )

	) terminals;;

(* check that we do not have duplicate constructors *)
let checkconst (g:Grammar.grammar) : unit = 
  match g with
      Grammar.Grammar(_,_,_,_,ps,_,_,_) ->
	let rec check ps =
	  match ps with
	      [] -> ()
	    | Grammar.Production(n,s,ss,ssop)::ps' ->
	      let (l1,ps') = List.partition (fun (Grammar.Production(n',s',ss',ssop')) -> n' = n) ps' in
	      if (l1 <> []) then (
		let err = 
		  "Error: the term constructor \""^n
		  ^"\" is used for two different productions.\n\n" 
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
    if len = 0 then sp^"   "
    else space (len-1) (sp^" ")
  in space len ""
;;

let print os (li:'a list) (longest:int) (c:int) : unit = 
  let rec print li i =
    match li with
	[] -> ()
      | hd::tl -> 
	if(i mod c == 0) then os "\n";
	Grammar.output_symbol os hd;
	let dif = longest-String.length (fst hd) in
	os (space dif);
	print tl (i+1)
  in print li 0 
;;


type args = string * string;; (* out file * grammar file *)

let sysargs : args =
  let rec getargs (pos:int) (args:args) : args =
    let a = Sys.argv.(pos) in
    if a = "-o" then
      Sys.argv.(pos+1),Sys.argv.(Array.length Sys.argv - 1)
    else if pos = Array.length Sys.argv - 1 then 
      fst args,Sys.argv.(pos)
    else getargs (pos+1) args
  in getargs 0 ("","")
;;



let () =
  (* If -v, -version, -help, --help or -usage are 
     flagged print usage and exit. *)
  Array.iter(fun a ->
    if a = "-version" || a = "-v" then (
      print_string vers; print_string "\n"; exit 1
    )
    else if a = "-help" || a = "--help" || a = "-usage" then (
      print_string usage; print_string "\n"; exit 1
    )
  ) Sys.argv;

  let gname = 
    if snd sysargs <> "" then snd sysargs
    else "<stdin>"
  in
  
  let parsed =
    let cin =
      if snd sysargs <> "" then
	open_in (snd sysargs)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    Parse.grammar Lex.token lexbuf
  in
  
  let os = 
    if (fst sysargs) <> "" then 
      output_string (open_out (fst sysargs))
    else print_string
  in

  
  let g = 
    match parsed with
      | Grammaru.Grammaru(None,_,_,_,_,_) -> Grammare.ge2g (Grammaru.gu2ge parsed)
      | Grammaru.Grammaru(Some imps,_,_,_,_,_) -> 
	let mergepr (l,r) (l',r') = (l,r@r') in
	let rec merge imps g files_used = 
	  match imps with
	    | [] -> g
	    | s::tl when List.exists ((=) s) files_used -> merge tl g files_used
	    | s::tl -> 
	      let parsed =
		let lexbuf = Lexing.from_channel (open_in ("../tests/"^s^".gra")) in
		Parse.grammar Lex.token lexbuf
	      in 
	      match g,parsed with
		| Grammaru.Grammaru(_,name,ss,pel,lcel,hs), Grammaru.Grammaru(Some x,_,_,pel',lcel',hs') -> 
		  let imp' = tl@x in
		  let g' =  Grammaru.Grammaru(None,name,ss,pel@pel',lcel@lcel',mergepr hs hs') in
		  merge imp' g' (s::files_used)
		| Grammaru.Grammaru(_,name,ss,pel,lcel,hs),Grammaru.Grammaru(None,_,_,pel',lcel',hs') -> 
		  let g' = Grammaru.Grammaru(None,name,ss,pel@pel',lcel@lcel',mergepr hs hs') in
		  merge tl g' (s::files_used)
	in Grammare.ge2g (Grammaru.gu2ge (merge imps parsed [fst sysargs]))
  in

  
  Grammar.check_for_keywords g;

  os ("\n\nGrammar for file "^gname^":\n\n");
  Grammar.output_grammar os g;


  os "\n\n\nNumber of Productions: ";
  os (string_of_int  (Grammar.num_productions g));

  
  os "\n\n\nNon-terminals:\n";
  let nonterminals = Grammar.get_nonterminals g in
  let length = longest nonterminals in
  print os nonterminals length 2;


  os "\n\n\nTerminals:\n";
  let terminals = (Grammar.get_terminals g) in
  let length = longest terminals in
  print os terminals length 5;


  os "\n\n\nStart Symbol: ";
  Grammar.output_symbol os (Grammar.get_start_symbol g);
  os "\n\n";

  checklex g terminals;
  checkconst g;
  dump_lexer g;
  dump_util g;
  dump_main g;
  dump_Makefile g;
  dump_syntax g;
  dump_parser g;
  dump_gviz g;
  dump_syntax_mode g;
  dump_mlcf_ppast g;
  dump_mlcf_pp g;
  dump_mlcf_eq g;
