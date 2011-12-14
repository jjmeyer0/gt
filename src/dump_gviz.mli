(** Takes a string s1 and checks s2 starts with s1.

	 @param sub a substring 
	 @param st a string to test
	 @return true if st starts with or is equal to sub, otherwise false. *)
val starts_with : string -> string -> bool

(** Takes a pair of string * bool and returns the string. This is
	 used to get the name of a symbol. 

	 @param (s,b) a pair representing symbol.
	 @return the name for given symbol var. *)
val symbol_var_name : (string*bool) -> string

(** Takes a list and maps a number to each element. 

	 @param xs a list to map numbers to.
	 @return returns a list that has a number mapped to each element. *)
val numbered : 'a list -> ('a * int) list

(** Takes a list of symbols and checks if the first one is a
	 nonterminal. 

	 @param ss a list of symbols
	 @return true if the hd of the list is a nonterminal otherwise false. *)
val starts_with_nonterminal : Grammar.Grammar.symbol list -> bool

(** Finds the position of symbol with name of given string. 

	 @param ss list of symbols to search for s
	 @param s find position of symbol with name s
	 @return an int representing the positions of string s in ss*)
val svn_pos : Grammar.Grammar.symbol list -> string -> int

(** The name to prefix functions. *)
val dump_nm : string

(** Dumps the function to print graph viz files to a file with the name
	 "graname_gviz.ml".

	 @param g a grammar on which gviz function is based. 
	 @return unit *)
val dump_gviz : Grammar.Grammar.grammar -> unit
