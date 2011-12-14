(** *)
val dump_lexer : Grammar.Grammar.grammar -> unit

(** *)
val dump_syntax : Grammar.Grammar.grammar -> unit

(** *)
val dump_parser : Grammar.Grammar.grammar -> unit

(** *)
val dump_main : Grammar.Grammar.grammar -> unit

(** *)
val dump_util : Grammar.Grammar.grammar -> unit

(** Creates a make file for needed for the generated parser. *)
val dump_Makefile : Grammar.Grammar.grammar -> unit
