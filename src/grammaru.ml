open Grammare
open Trie
open Elist
open Estring

module Grammaru = struct 
  type highlights = string * (string * string list) list;;
  type symbolu = string;;
  type elementu = 
      Esymbolu of symbolu 
    | Eoptionu of elementu list list
    | Erepetitionu of elementu list list * string * string;; 	
  type productionu = Productionu of string * string * elementu list * elementu list list option;;
  type lexclassu = string * (string * bool);;  (* (name_of_lexclass, definition_of_lexclass, is_token_in_ast) *)
  type grammaru = Grammaru of string list option * string * string option * productionu list * lexclassu list * highlights;;


  let rec get_nonterminal_strings (Grammaru(_,_,_,productions,_,_):grammaru) : 'a list =
    let prods : string list = List.map (fun (Productionu(o,s,ss,el_opts)) -> s) productions in
    List.unique prods
  ;;

  let get_start_symbol (g:grammaru) : string = 
    match g with
      | Grammaru(_,_,_,(Productionu(o,s,ss,el_ops)::ps),_,_) -> s
      | Grammaru(_,_,_,[],_,_) -> 
	let err = "\nThe given grammar does not define any productions.\n\n" in
	failwith err
  ;;

  let is_newlnsens (newlnsens:string option) : bool = 
    match newlnsens with 
      | Some(n) when n = "true" -> true
      | _ -> false
  ;;

  let rec su2se (r:unit trie) (s:symbolu) : (symbolu*bool) =
    match trie_lookup r s with
      | Some() -> s,false
      | None -> s,true
  ;;

  let rec esu2ese (t:unit trie) (esu:elementu list) : Grammare.element list = 
    match esu with
      | [] -> []
      | Esymbolu(sym)::esu' -> Grammare.Esymbol(su2se t sym)::esu2ese t esu'
      | Eoptionu(es')::esu' -> 
	let es'' = List.map(esu2ese t) es' in
	Grammare.Eoption(es'')::esu2ese t esu'
      | Erepetitionu(es',lr,gte)::esu' -> 
	let es'' = List.map(esu2ese t) es' in
	Grammare.Erepetition(es'',lr,gte)::esu2ese t esu'
  ;;

  let rec esuopt2eseopt (r:unit trie) (esu_opt:elementu list list option) : Grammare.element list list option = 
    match esu_opt with
      | None -> None
      | Some(e) -> Some(List.map(esu2ese r) e)
  ;;

  let rec pul2pel (r:unit trie) (pul:productionu list) : Grammare.productione list = 
    match pul with
      | [] -> []
      | Productionu(s,ss,esu,esu_opt)::pul' -> 
	let esu' = esu2ese r esu in
	let esu_opt' = esuopt2eseopt r esu_opt in
	Grammare.Productione(s,ss,esu',esu_opt')::(pul2pel r pul')
  ;;

  let rec gu2ge (Grammaru(ims,n,lcd,pl,lcl,hs) as g:grammaru) : Grammare.grammare = 
    let r = ref Tnone in
    let nonterminals = get_nonterminal_strings g in
    List.iter (fun n -> r := trie_insert !r n ()) nonterminals;
    let prods = pul2pel !r pl in 
    let prods = 
      List.sort (fun (Grammare.Productione(o,s,ss,ssopt)) 
	(Grammare.Productione(o,s',ss',ssopt')) -> compare s s'
      ) prods 
    in
    let lcs' = 
      List.map (fun (n,(d,b)) -> 
	if b then r := (trie_insert !r n ()); 
	(n,d)
      ) lcl 
    in
    let ss = get_start_symbol g in
    Grammare.Grammare(ims,n,ss,lcd,prods,lcs',!r,hs)
  ;;
end


