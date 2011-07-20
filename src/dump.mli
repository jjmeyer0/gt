
(** *)
val get_comment_delimiter : string -> string

(** *)
val get_ln_comment_delim : string option -> string

(** *)
val starts_with : string -> string -> bool

(** *)
val is_list : string -> bool

(** *)
val is_repetition : string -> bool

(** *)
val is_opt : string -> bool

(** *)
val symbol_var_name : string * bool -> string

(** *)
val starts_with_nonterminal : Grammar.symbol list -> bool

(** *)
val numbered : Grammar.symbol list -> (Grammar.symbol * int) list

(** *)
val svn_pos : Grammar.symbol list -> string -> int

(** *)
val dump_lexer : Grammar.grammar -> unit

(** *)
val dump_syntax : Grammar.grammar -> unit

(** *)
val print_parser_list : (string -> unit) -> (string * bool) list -> unit Trie.trie -> bool -> unit

(** *)
val print_parser_error : (string -> unit) -> string -> string -> unit

(** *)
val print_parser_nonterminals : (string -> unit) -> Grammar.symbol list -> string -> string -> string -> unit

(** *)
val dump_parser : Grammar.grammar -> unit

(** *)
val dump_main : Grammar.grammar -> unit

(** *)
val dump_util : Grammar.grammar -> unit

(** Creates a make file for needed for the generated parser. *)
val dump_Makefile : Grammar.grammar -> unit
