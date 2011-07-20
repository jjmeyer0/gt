open Trie;;
(* 
   Grammar(name,productions) defines the grammar with the
   given name and productions. 

   Production(n,s,[s1; ... ; sn]) is for the production

   n : s -> s1 ... sn

   where n is the name for the ctor for the AST. The s, s1, ..., sn are
   just strings.

*)
type highlights = string * (string * string list) list;;


type symbolu = string;;
type elementu = 
	 Esymbolu of symbolu 
  | Eoptionu of elementu list list
  | Erepetitionu of elementu list list * string * string;; 	
type productionu = Productionu of string * string * elementu list * ((elementu list) list) option;;
type lexclassu = string * (string * bool);;  (* (name_of_lexclass, definition_of_lexclass, is_token_in_ast) *)
type grammaru = Grammaru of string * string option * productionu list * lexclassu list * highlights;;



type symbole = string * bool (* true iff terminal *);;
type element = 
	 Esymbol of symbole 
  | Eoption of element list list
  | Erepetition of element list list * string (* left/right *) * string (* >=n *) ;; 

type productione = Productione of string * string * element list * ((element list) list) option;;
type lexclasse = string * string;; 
type grammare = Grammare of string (* name *) * string (* start symbol *) *  string option (* line comment *)
  * productione list * lexclasse list * unit trie  * highlights   (* the unit trie is for whether
																								 or not terminals are in the AST *);;

type symbol = string * bool (* true iff terminal *);;
type production = Production of string * string * symbol list * ((symbol list) list) option;;
type lexclass = string * string;; 
type grammar = Grammar of string (* name *) * string (* start symbol *) *  string option(* line comment *)
		* production list * lexclass list * unit trie * highlights  (* the unit trie is for whether
									     or not terminals are in the AST *);;


(*
========================================================
Start of Grammaru to Grammare
========================================================
*)
let is_terminal ((s,b):symbol) : bool = b;;
let sym_name ((s,b):symbol) : string = s;;

let is_newlnsens (newlnsens : string option) : bool = 
  match newlnsens with 
		None -> false 
	 | Some(n) -> if n = "true" then true else false

let rec su2se (r:unit trie) (s:symbolu) : (symbolu*bool) =
  (s,match trie_lookup r s with
      Some() -> false
    | None -> true)
;;

let rec unique (l:'a list) : 'a list =
  match l with
      [] -> []
    | [x] -> [x]
    | x::y::l -> 
		if (x = y) then unique (y::l) 
		else x::(unique (y::l))
;;

(* return a list of all the nonterminal symbols in g *)
let rec get_nonterminal_strings (g:grammaru) : 'a list =
  match g with
      Grammaru(_,_,productions,_,_) ->
		  let rec all_nonterminals ps =
			 match ps with
				  [] -> []
				| (Productionu(o,s,ss,el_opts))::ps' -> s::(all_nonterminals ps') 
		  in 
	     unique (List.sort (fun s1 s2 -> compare s1 s2) (all_nonterminals productions))
;;

(* return the start symbol of the grammar *)
let get_start_symbol (g:grammaru) : string = 
  match g with
		Grammaru(_,_,(Productionu(o,s,ss,el_ops)::ps),_,_) -> s
	 | Grammaru(_,_,[],_,_) -> 
		let err = "\nThe given grammar does not define any productions.\n\n" in
		failwith err
;;

(* elementu list to element list *)
let rec esu2ese (r:unit trie) (esu:elementu list) : element list = 
  match esu with
      [] -> []
    | e::esu' -> 
		let e' = 
		  match e with
				Esymbolu(sym) -> Esymbol(su2se r sym)
			 | Eoptionu(es') -> 
				let es'' = List.map(esu2ese r) es' in
				Eoption(es'')
			 | Erepetitionu(es',lr,gte) -> 
				let es'' = List.map(esu2ese r) es' in
				Erepetition(es'',lr,gte)
		in
		e'::esu2ese r esu'
;;

let rec esuopt2eseopt (r:unit trie) (esu_opt:(elementu list list) option) : (element list list) option = 
  match esu_opt with
		None -> None
	 | Some(e) ->
		let e' = List.map(esu2ese r) e in
		Some(e')
;;

(* Productionu list to Productione list *)
let rec pul2pel (r:unit trie) (pul:productionu list) : productione list = 
  match pul with
      [] -> []
    | p::pul' -> 
	   match p with 
          Productionu(s,ss,esu,esu_opt) -> 
				let esu' = esu2ese r esu in
				let esu_opt' = esuopt2eseopt r esu_opt in
				Productione(s,ss,esu',esu_opt')::(pul2pel r pul');;

let rec gu2ge (g:grammaru) : grammare = 
  match g with
      Grammaru(n,lcd,pl,lcl,hs) -> 
		  let r = ref Tnone in
		  let nonterminals = get_nonterminal_strings g in
		  List.iter (fun n -> r := trie_insert !r n ()) nonterminals;
		  let prods = pul2pel !r pl in 
		  let prods = List.sort (fun (Productione(o,s,ss,ssopt)) (Productione(o,s',ss',ssopt')) ->
			 compare s s') prods in
		  let lcs' = List.map (fun (n,(d,b)) -> if b then r := (trie_insert !r n ()); (n,d)) lcl in
		  let ss = get_start_symbol g in
    	  Grammare(n,ss,lcd,prods,lcs',!r,hs);;






(*
========================================================
Start of Grammare to Grammar
========================================================
*)
let rec generate_sym_nm (n:string) (c:string) (j:'a ref) (es:element list) : string*bool = 
  match es with
	 	[] -> let error =  "Error: incorrect match. \n\n" in failwith error
    | e::es' -> 
      match e with 
          Esymbol(e') -> 
				(match e' with (s,b) -> 
				  (String.lowercase (n^"_"^c^"_"^s^(string_of_int !j)),false));
		  | Eoption(e') -> generate_sym_nm n c j (List.hd e');
		  | Erepetition(e',lr,gte) -> generate_sym_nm n c j (List.hd e')
;;




   
let new_prods = ref [];;
let rec repetitions (n:int) (ss:symbol list) = 
  if(n>0) then repetitions (n-1) ss@ss
  else [];;

let rec e2s (i:int ref) (j:int ref) (s:string) (ss:string) (e:element) : symbol = 
  match e with
      Esymbol(s) -> s
    | Eoption(es) -> 
		let new_sym = generate_sym_nm s ss j (List.hd es) in 
		incr j;
		
		let p = 
		  List.map(fun es' ->
			 let ss = List.map(e2s i j s ss) es' in
			 Production("Option`"^(string_of_int !i),sym_name new_sym,ss,None) 
		  ) es 
		in
		
		incr i;
		let p2 = Production("Option`"^(string_of_int !i),sym_name new_sym,[],None) in
		incr i;
		new_prods := p2::p@(!new_prods); 
		new_sym
		  
    | Erepetition(es,lr,gte) -> 
		let new_sym = generate_sym_nm s ss j (List.hd es) in
		incr j;
		
		let pre = 
		  if(int_of_string gte > 1) then 
			 "List_"^(String.capitalize lr)^"_Repetition"^(string_of_int !i)
		  else 
			 "List_"^(String.capitalize lr)^(string_of_int !i)
		in
		
		let ss = List.map(e2s i j s ss) (List.hd es) in 
		
		let p = 
		  let nm = (sym_name new_sym) in
		  let reps = repetitions (int_of_string gte) ss in
		  Production(pre,nm,reps,None) 
		in
		incr i;
		
		let li = "List_"^(String.capitalize lr)^(string_of_int !i) in
		let nm = sym_name new_sym in
		let li' = if lr = "left" then new_sym::ss else ss@(new_sym::[]) in
	 	let p2 = Production(li,nm,li',None) in
		incr i;
		new_prods := p::p2::!new_prods;
		new_sym


let rec pel2pl (i:int ref) (j:int ref) (pel:productione list) : production list = 
  match pel with
      [] -> !new_prods
    | (Productione(s,ss,el,elop))::pel' ->
		let prods = ref [] in

		let ps el = 
		  List.map(fun e ->
			 match e with
				  Esymbol(s) -> Esymbol(s)
				| Eoption(es) -> 
				  if List.length es > 1 then (
					 let sym = generate_sym_nm s ss j (List.hd es) in
					 incr j;
					 prods := (Productione(s^"Med",fst sym,List.hd es,Some(List.tl es)))::(!prods);
					 let es = (Esymbol(sym))::[] in 
					 Eoption(es::[])
				  ) else e
				| Erepetition(es,lr,gte) -> 
				  if List.length es > 1 then (
					 let sym = generate_sym_nm s ss j (List.hd es) in
					 incr j;
					 prods := (Productione(s^"Med",fst sym,List.hd es,Some(List.tl es)))::(!prods);
					 let es = (Esymbol(sym))::[] in 
					 Erepetition(es::[],lr,gte)
				  ) else e
		  ) el 
		in 
		let e = List.map(e2s i j s ss) (ps el) in
		let elop = 
		  match elop with 
				None -> None 
			 | Some(elop) -> Some(List.map(fun e -> List.map(e2s i j s ss) (ps e)) elop) 
		in
		let p = Production(s,ss,e,elop) in
		p::pel2pl i j (!prods@pel')





let rec ge2g (g:grammare) : grammar = 
  match g with 
      Grammare(n,ss,lcd,pel,lcel,t,hs) -> 
		  let i = ref 0 in 	
		  let j = ref 0 in 
		  let pl = pel2pl i j pel in
        Grammar(n,ss,lcd,pl,lcel,t,hs);;

let rec num_productions (Grammar(_,_,_,ps,_,_,_):grammar) : int = List.length ps;;

let output_symbol (os:string->unit) ((s,_):string*'a) = os s;;

let rec output_productions (os:string->unit) (ps:production list) : unit =
  match ps with 
      [] -> ()
    | Production(n,s,ss,ssop)::ps -> 
		os (n^" : "^s^" ->");
		List.iter (fun s -> os " "; output_symbol os s) ss;
		os "\n";
		output_productions os ps;;

let output_lexclasses (os:string->unit) (t:unit trie) (cls:'a list) : unit = 
  List.iter (fun (s,c) -> 
	 let in_ast = trie_contains t s in
	 os "\n"; os s; os " = ";
	 if in_ast then os "{{ ";
	 os c;
	 if in_ast then os " }}"
  ) cls
;;

let output_grammar (os:string->unit) (g:grammar) : unit = 
  match g with 
      Grammar(s,_,_,ps,cls,t,_) -> 
		  os s; os "\n"; 
		  output_productions os ps;
		  output_lexclasses os t cls;
;;

(* return a list of all the symbols in g *)
let rec get_symbols (g:grammar) : symbol list =
  match g with
      Grammar(_,_,_,productions,_,_,_) ->
		  let rec all_symbols ps =
			 match ps with
				  [] -> []
				| (Production(o,s,ss,ssop))::ps' -> 
				  let ssop' = 
					 match ssop with
						  None -> []
						| Some(x) -> (List.flatten x)
				  in
				  let ss = ss@ssop' in
				  (s,false)::(List.append ss (all_symbols ps'))
		  in
		  let all_syms = all_symbols productions in
	  	  unique (List.sort (fun s1 s2 -> compare s1 s2) all_syms)
;;

let is_in_ast (t:unit trie) (s,b) : bool = (not b) (* not a terminal *) || (trie_contains t s);;

let string_of_terminal (Grammar(s,_,_,ps,cls,t,_)) (s,b) = List.assoc s cls;;

(* return a list of all the terminal symbols in g *)
let get_terminals (g:grammar) : symbol list =
  List.filter is_terminal (get_symbols g);;

(* return a list of all the terminal symbols in g *)
let get_nonterminals (g:grammar) : symbol list =
  List.filter (fun n -> not (is_terminal n)) (get_symbols g);;

(* return the start symbol of the grammar *)
let get_start_symbol (Grammar(_,s,_,_,_,_,_) : grammar) : symbol = (s,false);;
