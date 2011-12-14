open Grammar
open Trie
open Elist
open Estring


module Grammare = struct
  type highlights = string * (string * string list) list;;
  type symbole = string * bool (* true iff terminal *);;
  type element = 
      Esymbol of symbole 
    | Eoption of element list list
    | Erepetition of element list list * string (* left/right *) * string (* >=n *) ;; 
  type productione = Productione of string * string * element list * element list list option;;
  type lexclasse = string * string;; 
  type grammare = Grammare of string list option * string (* name *) * string (* start symbol *) *  string option (* line comment *)
    * productione list * lexclasse list * unit trie  * highlights   (* the unit trie is for whether
								       or not terminals are in the AST *);;
  let is_terminal (s:symbole) : bool = snd s;;
  let sym_name (s:symbole) : string = fst s;;
  let is_in_ast (t:unit trie) (s,b) : bool = (not b) (* not a terminal *) || (trie_contains t s);;




  (*========================================================
    Start of Grammare to Grammar
    ========================================================*)
  let rec generate_sym_nm (n:string) (c:string) (j:int) (es:element list) : string*bool = 
    match es with
      | [] -> let error = "Error: incorrect match. \n\n" in failwith error
      | Esymbol(e')::es' -> String.lowercase (n^"_"^c^"_"^fst e'^string_of_int j),false;
      | Eoption(e')::es' -> generate_sym_nm n c j (List.hd e');
      | Erepetition(e',lr,gte)::es' -> generate_sym_nm n c j (List.hd e')
  ;;

  let new_prods = ref [];;

  let rec e2s (i:int ref) (j:int ref) (s:string) (ss:string) (e:element) : Grammar.symbol = 
    match e with
      | Esymbol(s) -> s
      | Eoption(es) -> 
	let new_sym = generate_sym_nm s ss !j (List.hd es) in 
	incr j;
	
	let p = 
	  List.map(fun es' ->
	    let ss = List.map(e2s i j s ss) es' in
	    Grammar.Production("Option`"^(string_of_int !i),sym_name new_sym,ss,None) 
	  ) es 
	in
	
	incr i;
	let p2 = Grammar.Production("Option`"^(string_of_int !i),sym_name new_sym,[],None) in
	incr i;
	new_prods := p2::p@(!new_prods); 
	new_sym
	  
      | Erepetition(es,lr,gte) -> 
	let new_sym = generate_sym_nm s ss !j (List.hd es) in
	incr j;
	let pre : string = 
	  let cap : string = String.capitalize lr in
	  let i : string = string_of_int !i in
	  if int_of_string gte > 1 then "List_"^cap^"_Repetition"^i
	  else "List_"^cap^i
	in
	
	let ss = List.map(e2s i j s ss) (List.hd es) in 
	
	let p = 
	  let nm = sym_name new_sym in
	  let reps = List.repetitions (int_of_string gte) ss in
	  Grammar.Production(pre,nm,reps,None) 
	in
	incr i;
	
	let li = "List_"^(String.capitalize lr)^(string_of_int !i) in
	let nm = sym_name new_sym in
	let li' = if lr = "left" then new_sym::ss else ss@(new_sym::[]) in
	let p2 = Grammar.Production(li,nm,li',None) in
	incr i;
	new_prods := p::p2::!new_prods;
	new_sym
  ;;

  let rec pel2pl (i:int ref) (j:int ref) (pel:productione list) : Grammar.production list = 
    match pel with
      | [] -> !new_prods
      | Productione(s,ss,el,elop)::pel' ->
	let prods = ref [] in

	let ps el = 
	  List.map(fun e ->
	    match e with
		Esymbol(s) -> Esymbol(s)
	      | Eoption(es) -> 
		if List.length es > 1 then (
		  let sym = generate_sym_nm s ss !j (List.hd es) in
		  incr j;
		  prods := Productione(s^"Med",fst sym,List.hd es,Some(List.tl es))::(!prods);
		  let es = Esymbol(sym)::[] in 
		  Eoption(es::[])
		) else e
	      | Erepetition(es,lr,gte) -> 
		if List.length es > 1 then (
		  let sym = generate_sym_nm s ss !j (List.hd es) in
		  incr j;
		  prods := Productione(s^"Med",fst sym,List.hd es,Some(List.tl es))::(!prods);
		  let es = Esymbol(sym)::[] in 
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
	let p = Grammar.Production(s,ss,e,elop) in
	p::pel2pl i j (!prods@pel')
  ;;


  let rec ge2g (Grammare(ims,n,ss,lcd,pel,lcel,t,hs):grammare) : Grammar.grammar = 
    let i = ref 0 in
    let j = ref 0 in
    let pl : Grammar.production list = pel2pl i j pel in
    Grammar.Grammar(ims,n,ss,lcd,pl,lcel,t,hs)
  ;;

end
