type comparison = Less | Greater | Equal

module type ORDERED_TYPE = sig
  type t
  val compare: t -> t -> comparison
  val print: t -> unit
  val length : t -> int
end

module Nonterminal = struct
  type t = string

  let compare n n' = 
    if n = n' then Equal
    else if n < n' then Less
    else Greater
  ;;

  let make s = s
  let name n = n
  let length n = String.length n
  let print n = print_string n
end

module Terminal = struct
  type t = string

  let compare t t' = 
    if t = t' then Equal
    else if t < t' then Less
    else Greater
  ;;

  let make s = s
  let name t = t
  let length t = String.length t
  let print t = print_string t
end

module Symbol = struct
  type t = 
    | Terminal of Terminal.t
    | Nonterminal of Nonterminal.t

  let compare s s' = match s,s' with
    | Nonterminal n,Nonterminal n' -> Nonterminal.compare n n'
    | Terminal t,Terminal t' -> Terminal.compare t t'
    | Nonterminal _,Terminal _ -> Greater
    | Terminal _,Nonterminal _ -> Less
  ;;

  let make s is_terminal =
    if is_terminal then Terminal (Terminal.make s)
    else Nonterminal (Nonterminal.make s)

  let length = function
    | Terminal t -> Terminal.length t
    | Nonterminal n -> Nonterminal.length n

  let terminal = function
    | Terminal _ -> true
    | Nonterminal _ -> false

  let nonterminal s = not (terminal s)

  let name = function
    | Terminal t -> Terminal.name t
    | Nonterminal n -> Nonterminal.name n

  let print = function
    | Terminal t -> Terminal.print t
    | Nonterminal n -> Nonterminal.print n
end

module Set = functor (Elt: ORDERED_TYPE) -> struct
  type t = Elt.t
  type set = t list

  let empty = []

  let rec add e s = match s with
    | [] -> [e]
    | hd::tl -> 
      match Elt.compare e hd with
	| Equal -> s
	| Less -> e::s
	| Greater -> hd::(add e tl)
  ;;

  let rec member e s = match s with
    | [] -> false
    | hd::tl -> 
      match Elt.compare e hd with
	| Equal -> true
	| Less -> false
	| Greater -> member e tl
  ;;

  let rec longest ?len:(l=0) s = match s with
    | [] -> l
    | hd::tl when Elt.length hd > l -> longest ~len:(Elt.length hd) tl
    | hd::tl -> longest ~len:l tl
  ;;

  let rec print_spaces i =
    if i > 1 then (
      print_string " ";
      print_spaces (i-1)
    )
  ;;
  
  let prettyprint s =
    let longest = longest s + 5 in
    let rec print i s = match s with
      | [] -> print_string "\n"
      | hd::tl ->
	Elt.print hd;
	print_spaces (longest - (Elt.length hd));
	if i mod 4 = 0 then print_string "\n";
	print (i+1) tl
    in
    print 1 s;
  ;;

  let print syms = List.iter (fun s -> Elt.print s; print_string " ") syms
end


module TerminalSet = Set (Terminal)
module NonterminalSet = Set (Nonterminal)
module SymbolSet = Set (Symbol)

module Production = struct
  type t = string * string * Symbol.t list

  let make constructor type_name symbols = (constructor,type_name,symbols)
  let constructor (c,_,_) = c
  let type_name (_,n,_) = n
  let symbols (_,_,symbols) = symbols
  let print (c,n,symbols) = 
    print_string (c^" : "^n^" -> ");
    SymbolSet.print symbols
  ;;

  let update con' = function 
    | (con,tp,syms) when String.contains con '`' -> (con',tp,syms)
    | (con,tp,syms) -> (con,tp,syms)
end


module LexClass = struct
  type id = string
  type t = id * Symbol.t

  let make l r = l,r
  let print (l,r) = print_string (l^" = "^r)
end

module Grammar = struct
  type imports = string list option
  type name = string
  type start_symbol = string
  type line_comment = string option
  type t = imports * name * start_symbol * line_comment * Production.t list * LexClass.t list

  let merge g g' = ()
  let make imports name start_symbol line_comment prods lexes = 
    imports,name,start_symbol,line_comment,prods,lexes
  let print (_,name,_,_,prods,lexes) =
    List.iter (fun s -> Production.print s; print_string ".\n") prods;
    print_string "\n";
    List.iter (fun s -> LexClass.print s; print_string "\n") lexes

  let update_prods (i,nm,ss,ln,prods,lexes) = 
    (i,nm,ss,ln,List.map (Production.update "AHHH") prods,lexes)
  ;;

  let check_lex g = ()
  let check_constructors g = ()
  let check_keywords g = ()
end

(*
let _ =
  let grammar = 
    let prods = 
      Production.make "Expr" "expr" [Symbol.make "ID" true; Symbol.make "PLUS" true; Symbol.make "expr" false] :: 
	Production.make "`ChangeExpr" "expr" [Symbol.make "ID" true; Symbol.make "MINUS" true; Symbol.make "expr" false] :: 
	Production.make "`ChangeExpr" "expr" [Symbol.make "ID" true; Symbol.make "MULT" true; Symbol.make "expr" false] :: 
	Production.make "`ChangeExpr" "expr" [Symbol.make "ID" true; Symbol.make "DIV" true; Symbol.make "expr" false] :: 
	Production.make "Id" "expr" [Symbol.make "ID" true] :: []
    in
    let lexes =
      LexClass.make "PLUS" "+" :: LexClass.make "MINUS" "-" :: LexClass.make "MULT" "*" ::
      LexClass.make "DIV" "/" :: LexClass.make "ID" "x" :: []
    in
    Grammar.make None "simple" "expr" None prods lexes
  in
  Grammar.print (Grammar.update_prods grammar)
  
  *)

