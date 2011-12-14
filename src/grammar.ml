open Trie
open Elist
open Estring

module Grammar = struct
  type highlights = string * (string * string list) list;;
  type symbol = string * bool (* true iff terminal *);;
  type production = Production of string * string * symbol list * symbol list list option;;
  type lexclass = string * string;; 
  type grammar = Grammar of string list option * string (* name *) * string (* start symbol *) * string option (* line comment *)
    * production list * lexclass list * unit trie * highlights  (* the unit trie is for whether
								   or not terminals are in the AST *);;

  let get_ln_comment_delim (lcd:string option) : string =
    match lcd with
	None -> "#"
      | Some(s)-> String.sub s 1 (String.length s - 1)
  ;;

  let is_list (ctornm:string) : bool = 
    String.starts_with "List_Left" ctornm ||
      String.starts_with "List_Right" ctornm
  ;;

  let is_repetition (ctor_nm:string) : bool =
    String.starts_with "List_Left_Repetition" ctor_nm ||
      String.starts_with "List_Right_Repetition"	ctor_nm
  ;;

  let is_opt (ctornm:string) : bool = String.starts_with "Option`" ctornm;;

  let is_terminal (s:symbol) : bool = snd s;;

  let starts_with_nonterminal (ss:symbol list) : bool =
    match ss with
	[] -> false
      | s::ss' -> not (is_terminal s)
  ;;

  let sym_name (s:symbol) : string = fst s;;

  let svn_pos (ss:symbol list) (s:string) : int = 
    let n = ref 0 in 
    let n' = ref 0 in
    if not (starts_with_nonterminal ss) then incr n;
    List.iter(fun s' ->
      incr n;
      if (sym_name s') = sym_name (s,false) then (n' := !n);
    ) ss; !n'
  ;;

  let is_in_ast (t:unit trie) (s,b) : bool = (not b) (* not a terminal *) || (trie_contains t s);;

  let string_of_terminal (Grammar(_,s,_,_,ps,cls,t,_)) (s,b) = List.assoc s cls;;

  let rec num_productions (Grammar(_,_,_,_,ps,_,_,_):grammar) : int = List.length ps;;

  (* return a list of all the symbols in g *)
  let rec get_symbols (Grammar(_,_,_,_,productions,_,_,_):grammar) : symbol list =
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
    List.unique all_syms
  ;;

  (* return a list of all the terminal symbols in g *)
  let get_terminals (g:grammar) : symbol list = List.filter is_terminal (get_symbols g);;

  (* return a list of all the terminal symbols in g *)
  let get_nonterminals (g:grammar) : symbol list = List.filter (fun n -> not (is_terminal n)) (get_symbols g);;

  (* return the start symbol of the grammar *)
  let get_start_symbol (Grammar(_,_,s,_,_,_,_,_) : grammar) : symbol = (s,false);;

  let output_symbol (os:string->unit) ((s,_):symbol) = os s;;
  
  let output_lexclasses (os:string->unit) (t:unit trie) (cls:'a list) : unit = 
    List.iter (fun (s,c) -> 
      let in_ast = trie_contains t s in
      os "\n"; os s; os " = ";
      if in_ast then os "{{ ";
      os c;
      if in_ast then os " }}"
    ) cls
  ;;

  let  output_productions (os:string->unit) (ps:production list) : unit =
    List.iter (fun (Production(n,s,ss,ssop)) ->
      os (n^" : "^s^" ->");
      List.iter (fun s -> os " "; output_symbol os s) ss;
      os "\n";
    ) ps
  ;;

  let output_grammar (os:string->unit) (Grammar(_,s,_,_,ps,cls,t,_):grammar) : unit = 
    os s; os "\n"; 
    output_productions os ps;
    output_lexclasses os t cls;
  ;;

end
