open Trie;;
(* 

   Grammar(name,productions) defines the grammar with the
   given name and productions. 

   Production(n,s,[s1; ... ; sn]) is for the production

   n : s -> s1 ... sn

   where n is the name for the ctor for the AST. The s, s1, ..., sn are
   just strings.

*)

type symbolu = string;;
type elementu = Esymbolu of symbolu | Eoptionu of elementu list | Erepetitionu of elementu list * string * string;; 	
type productionu = Productionu of string * string * elementu list;;
type lexclassu = string * (string * bool);;  (* (name_of_lexclass, definition_of_lexclass, is_token_in_ast) *)
type grammaru = Grammaru of string * string option * productionu list * lexclassu list * string;;

type symbole = string * bool (* true iff terminal *);;
type element = Esymbol of symbole | Eoption of element list | Erepetition of element list * string (* left/right *) * string (* >=n *) ;; 
type productione = Productione of string * string * element list;;
type lexclasse = string * string;; 
type grammare = Grammare of (string (* name *) * string (* start symbol *) * string option (* line comment *)
		* productione list * lexclasse list * unit trie * string)   (* the unit trie is for whether
									     or not terminals are in the AST *);;

type symbol = string * bool (* true iff terminal *);;
type production = Production of string * string * symbol list;;
type lexclass = string * string;; 
type grammar = Grammar of (string (* name *) * string (* start symbol *) * string option(* line comment *)
		* production list * lexclass list * unit trie * string)   (* the unit trie is for whether
									     or not terminals are in the AST *);;
(*
========================================================
Start of Grammaru to Grammare
========================================================
*)
let is_terminal (s,b) = b;;

let rec su2se (r:unit trie) s =
  (s,match trie_lookup r s with
       Some() -> false
     | None -> true);;

let rec unique l =
  match l with
      [] -> []
    | [x] -> [x]
    | x::y::l -> if (x = y) then unique (y::l) else x::(unique (y::l));;

(* return a list of all the nonterminal symbols in g *)
let rec get_nonterminal_strings (g:grammaru) =
  match g with
      Grammaru(_,_,productions,_,_) ->
	let rec all_nonterminals ps =
	  match ps with
	      [] -> []
	    | (Productionu(o,s,ss))::ps' -> s::(all_nonterminals ps') in 
	         unique (List.sort (fun s1 s2 -> compare s1 s2) (all_nonterminals productions));;

(* return the start symbol of the grammar *)
let get_start_symbol (g:grammaru) = 
  match g with
      Grammaru(_,_,(Productionu(o,s,ss)::ps),_,_) -> s
    | Grammaru(_,_,[],_,_) -> print_string "Encountered grammar with empty set of productions, which should not happen.\n"; exit 1;;

(* elementu list to element list *)
let rec esu2ese r esu = 
   match esu with
      [] -> [];
      | e::esu' -> 
	(match e with
	   Esymbolu(sym) -> Esymbol(su2se r sym)
	   | Eoptionu(es') -> Eoption(esu2ese r es')
	   | Erepetitionu(es',lr,gte) -> Erepetition(esu2ese r es',lr,gte))::esu2ese r esu';;

(* Productionu list to Productione list *)
let rec pul2pel r pul = 
   match pul with
      [] -> []
      | p::pul' -> 
	   match p with 
              Productionu(s,ss,esu) -> Productione(s,ss,esu2ese r esu)::(pul2pel r pul');;

let rec gu2ge g = 
   match g with
      Grammaru(n,lcd,pl,lcl,ed) -> 
	let r = ref Tnone in
	let nonterminals = get_nonterminal_strings g in
	  List.iter (fun n -> 
		       r := trie_insert !r n ()) nonterminals;
	let prods = pul2pel !r pl in 
	let prods = List.sort (fun (Productione(o,s,ss)) (Productione(o,s',ss')) ->
				   compare s s') prods in
	let lcs' = List.map (fun (n,(d,b)) -> if b then r := (trie_insert !r n ()); (n,d)) lcl in
        let ss = get_start_symbol g in
    	Grammare(n,ss,lcd,prods,lcs',!r,ed);;

(*
========================================================
Start of Grammare to Grammar
========================================================
*)
let rec generate_sym_nm n c j es = 
   match es with
	 	[] -> print_string "Error: incorrect match. \n\n"; exit 1;
      | e::es' -> 
         match e with 
            Esymbol(e') -> 
					(match e' with (s,b) -> 
						(String.lowercase (n^"_"^c^"_"^s^(string_of_int !j)),false));
				| Eoption(e') -> generate_sym_nm n c j e'; (*print_string "Error: incorrect match. \n\n"; exit 1;*)
				| Erepetition(e',lr,gte) -> generate_sym_nm n c j e';; (*print_string "Error: incorrect match. \n\n"; exit 1;;*)

let rec sym_name s = 
   match s with (s,b) -> s;;
   
let new_prods = ref [];;

let rec repetitions n ss = 
   if(n>0) then
      (repetitions (n-1) ss@ss)
   else [];;

let rec e2s i j s ss e = 
   match e with
      Esymbol(s) -> s
      | Eoption(es) ->
         let new_sym = generate_sym_nm s ss j es in
			j := !j+1;
         let ss = List.map(e2s i j s ss) es in
         let p = Production("Option`"^(string_of_int !i),sym_name new_sym,ss) in
         i := !i+1;
         let p2 = Production("Option`"^(string_of_int !i),sym_name new_sym,[]) in
			i := !i+1;
         new_prods := p::p2::!new_prods; new_sym;
      | Erepetition(es,lr,gte) -> 
         let new_sym = generate_sym_nm s ss j es in
			j := !j+1;
         let ss = List.map(e2s i j s ss) es in
			let pre = if(int_of_string gte > 1) then "List_"^(String.capitalize lr)^"_Repetition" else "List_"^(String.capitalize lr) in
         let p = Production(pre^(string_of_int !i),(sym_name new_sym), repetitions (int_of_string gte) ss) in
         i := !i+1;
	 		let p2 = Production("List_"^(String.capitalize lr)^(string_of_int !i),(sym_name new_sym),if(lr = "right") then (ss@(new_sym::[])) else (new_sym::ss)) in
			i := !i+1;
         new_prods := p::p2::!new_prods; new_sym;;

let rec pel2pl i j pel = 
   match pel with
      [] -> !new_prods
      | p::pel' ->
         let p' = 
			(match p with 
				Productione(s,ss,el) -> 
		    	Production(s,ss,List.map(e2s i j s ss) el)) in
   	      	i := !i+1; 
					j := !j+1; 
					p'::pel2pl i j pel';;

(* lexclasse list to lexclass list *)
let rec lel2ll ll = 
   match ll with
      [] -> [];
      | l::ll' -> (match l with (s,s') -> (s,s'))::(lel2ll ll');;

let rec ge2g g = 
   match g with 
      Grammare(n,ss,lcd,pel,lcel,t,ed) ->
			let pl = 
				(let i = ref 0 in 	
				let j = ref 0 in 
				pel2pl i j pel) in
	 		let ll = lel2ll lcel in
         Grammar(n,ss,lcd,pl,ll,t,ed);;

let rec num_productions g =
  match g with
      Grammar(_,_,_,ps,_,_,_) -> List.length ps;;

let output_symbol o (s,_) = output_string o s;;

let rec output_productions o ps =
  match ps with 
      [] -> ()
    | Production(n,s,ss)::ps -> 
	output_string o n;
	output_string o " : ";
	output_string o s;
	output_string o " ->";
	List.iter (fun s -> output_string o " "; output_symbol o s) ss;
	output_string o "\n";
	output_productions o ps;;

let output_lexclasses o t cls = 
  List.iter (fun (s,c) -> 
	       let in_ast = trie_contains t s in
		 output_string o "\n";
		 output_string o s;
		 output_string o " = ";
		 if in_ast then output_string o "{{ ";
		 output_string o c;
		 if in_ast then output_string o " }}")
    cls;;

let output_grammar (o:out_channel) (g:grammar) = 
  match g with 
      Grammar(s,_,_,ps,cls,t,extradata) -> 
	output_string o s;
	output_string o "\n"; 
	output_productions o ps;
	output_lexclasses o t cls;
	if extradata <> "" then
	  (output_string o "\n\n{{" ;
	   output_string o extradata ;
	   output_string o "}}" );;

(* return a list of all the symbols in g *)
let rec get_symbols (g:grammar) =
  match g with
      Grammar(_,_,_,productions,_,_,_) ->
	let rec all_symbols ps =
	  match ps with
	      [] -> []
	    | (Production(o,s,ss))::ps' -> (s,false)::(List.append ss (all_symbols ps')) in
	  		unique (List.sort (fun s1 s2 -> compare s1 s2) (all_symbols productions));;

let is_in_ast t (s,b) = (not b) (* not a terminal *) || (trie_contains t s);;

let string_of_terminal (Grammar(s,_,_,ps,cls,t,_)) (s,b) = 
  List.assoc s cls;;

(* return a list of all the terminal symbols in g *)
let get_terminals (g:grammar) =
  List.filter is_terminal (get_symbols g);;

(* return a list of all the terminal symbols in g *)
let get_nonterminals (g:grammar) =
  List.filter (fun n -> not (is_terminal n)) (get_symbols g);;

(* return the start symbol of the grammar *)
let get_start_symbol (Grammar(_,s,_,_,_,_,_)) = (s,false);;
