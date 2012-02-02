module Grammar :
  sig
    type highlights = string * (string * string list) list
    type symbol = string * bool
    type production =
        Production of string * string * symbol list * symbol list list option
    type lexclass = string * string
    type grammar =
        Grammar of string list option * string * string * string option *
          production list * lexclass list * unit Trie.trie * highlights
    val get_ln_comment_delim : string option -> string
    val is_list : string -> bool
    val is_repetition : string -> bool
    val is_opt : string -> bool
    val is_terminal : symbol -> bool
    val starts_with_nonterminal : symbol list -> bool
    val sym_name : symbol -> string
    val svn_pos : symbol list -> string -> int
    val is_in_ast : unit Trie.trie -> string * bool -> bool
    val string_of_terminal : grammar -> string * 'a -> string
    val num_productions : grammar -> int
    val get_symbols : grammar -> symbol list
    val get_terminals : grammar -> symbol list
    val get_nonterminals : grammar -> symbol list
    val get_start_symbol : grammar -> symbol
    val output_symbol : (string -> unit) -> symbol -> unit
    val output_lexclasses :
      (string -> unit) -> unit Trie.trie -> (string * string) list -> unit
    val output_productions : (string -> unit) -> production list -> unit
    val output_grammar : (string -> unit) -> grammar -> unit
    val check_for_keywords : grammar -> unit
  end
