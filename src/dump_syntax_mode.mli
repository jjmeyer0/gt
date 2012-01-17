val special_chars : char list
val is_in_ast : unit Trie.trie -> string -> bool
val delim : string option -> string
val get_comment_style : string option -> string
val get_gedit : string -> string * string
val get_emacs : string -> string
val dump_gedit : Grammar.Grammar.grammar -> unit
val dump_emacs : Grammar.Grammar.grammar -> unit
val syncolor' : string -> unit Trie.trie -> (string * string) list -> string * (string * string list) list
val dump_syntax_mode : Grammar.Grammar.grammar -> unit
