module Grammare :
  sig
    type highlights = string * (string * string list) list
    type symbole = string * bool
    type element =
        Esymbol of symbole
      | Eoption of element list list
      | Erepetition of element list list * string * string
    type productione =
        Productione of string * string * element list *
          element list list option
    type lexclasse = string * string
    type grammare =
        Grammare of string * string * string option * productione list *
          lexclasse list * unit Trie.trie * highlights
    val is_terminal : symbole -> bool
    val sym_name : symbole -> string
    val is_in_ast : unit Trie.trie -> string * bool -> bool
    val generate_sym_nm :
      string -> string -> int -> element list -> string * bool
    val new_prods : Grammar.Grammar.production list ref
    val e2s :
      int ref ->
      int ref -> string -> string -> element -> Grammar.Grammar.symbol
    val pel2pl :
      int ref ->
      int ref -> productione list -> Grammar.Grammar.production list
    val ge2g : grammare -> Grammar.Grammar.grammar
  end
