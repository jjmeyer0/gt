type highlights = string * (string * string list) list
type symbolu = string
type elementu =
    Esymbolu of symbolu
  | Eoptionu of elementu list list
  | Erepetitionu of elementu list list * string * string
type productionu =
    Productionu of string * string * elementu list *
      elementu list list option
type lexclassu = string * (string * bool)
type grammaru =
    Grammaru of string * string option * productionu list * lexclassu list *
      highlights
type symbole = string * bool
type element =
    Esymbol of symbole
  | Eoption of element list list
  | Erepetition of element list list * string * string
type productione =
    Productione of string * string * element list * element list list option
type lexclasse = string * string
type grammare =
    Grammare of string * string * string option * productione list *
      lexclasse list * unit Trie.trie * highlights
type symbol = string * bool
type production =
    Production of string * string * symbol list * symbol list list option
type lexclass = string * string
type grammar =
    Grammar of string * string * string option * production list *
      lexclass list * unit Trie.trie * highlights
val is_terminal : symbol -> bool
val sym_name : symbol -> string
val is_newlnsens : string option -> bool
val su2se : unit Trie.trie -> symbolu -> symbolu * bool
val unique : 'a list -> 'a list
val get_nonterminal_strings : grammaru -> string list
val esu2ese : unit Trie.trie -> elementu list -> element list
val esuopt2eseopt : unit Trie.trie -> elementu list list option -> element list list option
val pul2pel : unit Trie.trie -> productionu list -> productione list
val gu2ge : grammaru -> grammare
val generate_sym_nm : string -> string -> int ref -> element list -> string * bool
val new_prods : production list ref
val repetitions : int -> symbol list -> symbol list
val e2s : int ref -> int ref -> string -> string -> element -> symbol
val pel2pl : int ref -> int ref -> productione list -> production list
val ge2g : grammare -> grammar
val num_productions : grammar -> int
val output_symbol : (string -> unit) -> string * 'a -> unit
val output_productions : (string -> unit) -> production list -> unit
val output_lexclasses : (string -> unit) -> unit Trie.trie -> (string * string) list -> unit
val output_grammar : (string -> unit) -> grammar -> unit
val get_symbols : grammar -> symbol list
val is_in_ast : unit Trie.trie -> string * bool -> bool
val string_of_terminal : grammar -> string * 'a -> string
val get_terminals : grammar -> symbol list
val get_nonterminals : grammar -> symbol list
val get_start_symbol : grammar -> symbol
