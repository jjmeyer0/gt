
module rec Esymbolu : sig
  type t = string
  val to_symbol : t -> Grammar.Symbol.t
  val to_string : t -> t
end = struct
  type t = string
  let to_symbol sym = Grammar.Symbol.make sym true ;;
  let to_string s = s
end

and Eoptionu : sig
  type t = Elementu.t list list
  val to_symbol : t -> Grammar.Symbol.t list list
  val to_string : t -> string
end = struct
  type t = Elementu.t list list
  let to_symbol elements = [[]];;
  let to_string els = 
    let st = List.map (fun es -> List.map (Elementu.to_string) es) els in
    ("[ " ^ (List.fold_left (^) " | " (List.flatten st)) ^ " ]")
  ;;
end

and Erepetitionu : sig
  type assoc = Left | Right
  type reps = int
  type t = Elementu.t list list * assoc * reps
  val left : assoc
  val right : assoc
  val make : Elementu.t list list -> assoc -> reps -> t
  val erepu2element : t -> Grammar.Symbol.t list list
  val to_string : t -> string
end = struct
  type assoc = Left | Right
  type reps = int
  type t = Elementu.t list list * assoc * reps
  let left = Left
  let right = Right
  let make (e:Elementu.t list list) (a:assoc) (r:reps) : t = e,a,r
  let erepu2element (elements,s,s') = [[]];;
  let to_string (els,assoc,reps) =
    let st = List.map (fun es -> List.map (Elementu.to_string) es) els in
    let syms = List.fold_left (^) " | " (List.flatten st) in
    let reps = match reps with
      | 0 when assoc = Right -> "*"
      | 1 when assoc = Right -> "+"
      | n -> 
	let assoc' = match assoc with 
	  | Left -> "left" 
	  | Right -> "right" 
	in "("^assoc'^",>="^string_of_int n^")"
    in
    "{ "^syms^" }"^reps
  ;;
end

and Elementu : sig
  type t =
    | Esymbolu of Esymbolu.t
    | Eoptionu of Eoptionu.t
    | Erepetitionu of Erepetitionu.t

  val make_esymbolu : Esymbolu.t -> t
  val make_erepu : Erepetitionu.t -> t
  val make_eoptu : Eoptionu.t -> t
  val to_string : t -> string
end = struct
  type t =
    | Esymbolu of Esymbolu.t
    | Eoptionu of Eoptionu.t
    | Erepetitionu of Erepetitionu.t

  let to_string = function
    | Esymbolu s -> Esymbolu.to_string s
    | Eoptionu e -> Eoptionu.to_string e
    | Erepetitionu e -> Erepetitionu.to_string e

  let make_esymbolu e = Esymbolu e
  let make_erepu e = Erepetitionu e
  let make_eoptu e = Eoptionu e
end

module LexClassu = struct
  type t = string * (string * bool)

  let to_lexclass (s,(s',b)) = (s,Grammar.Symbol.make s' b)

  let print (nm,(v,_)) = Printf.printf "%s = %s\n" nm v
end

module Productionu = struct
  type t = string * string * Elementu.t list * Elementu.t list list option

  let print (c,t,body,opts) =
    Printf.printf "%s : %s ->" c t;
    let print_body body =
      List.iter (fun e -> Printf.printf " %s" (Elementu.to_string e)) body;
    in
    let print_opts = function
      | None -> ()
      | Some opts -> 
	List.iter (fun e ->
	  Printf.printf " | ";
	  print_body e;
	) opts;
    in 
    print_body body;
    print_opts opts; 
    Printf.printf " .\n"
  ;;

  let make con tp es eopts = con,tp,es,eopts

  let to_production (con,tpnm,elements,elementsopt) = con,tpnm,elements;;

  let update_name prod =
    print prod;
    Printf.printf "\n\nEnter new constructor name for above: ";
    read_line ()
  ;;

  let to_bnf prod = match prod with
    | (con,tpnm,elements,None) -> [prod]
    | (con,tpnm,elements,Some opts) as p ->
      Printf.printf "Updating this production: ";
      print p;
      Printf.printf "\n\n";
      (update_name p,tpnm,elements,None)::
	(List.map (fun es ->
	  let new_prod = (update_name ("___",tpnm,es,None),tpnm,es,None) in 
	  Printf.printf "\n\n";
	  new_prod
	) opts)
  ;;
end

module Grammaru = struct
  type imports = string list option
  type comment_delimeter = string option
  type t = imports * string * comment_delimeter * Productionu.t list * LexClassu.t list

  let make imps name cmt prods lexes = imps,name,cmt,prods,lexes

  let start_symbol (_,_,_,prods,_) = match prods with
    | [] -> ""
    | (_,tp,_,_)::_ -> tp
  ;;

  let to_grammar ((imports,gname,cdelim,prods,lexes) as g) =
    let prods = List.map Productionu.to_production prods in
    let lexes = List.map LexClassu.to_lexclass lexes in
    imports,gname,start_symbol g,cdelim,prods,lexes
  ;;

  let to_bnf (imports,gname,cdelim,prods,lexes) =
    let prods = List.flatten (List.map (Productionu.to_bnf) prods) in
    imports,gname,cdelim,prods,lexes
  ;;

  let prettyprint ((imports,name,cd,prods,lexes):t) =
    let cd = match cd with | None -> "\"#\"" | Some x -> x in
    Printf.printf "%s\n\nline_comment = %s\n\n" name cd;
    List.iter Productionu.print prods;
    Printf.printf "\n\n";
    List.iter LexClassu.print lexes
  ;;
end
