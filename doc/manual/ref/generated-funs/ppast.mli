(** A reference to keep track of the current line number. *)
val cur_line : int ref

(** This function will print the right number of newlines in the correct
	 places for any parseable file. 

	 @param os where to print the information
	 @param do_print if true newlines will be printed otherwise nothing will happen.
	 @param p if do_print is true then the difference between p and cur_line newlines will be printed.
	 @return unit *)
val print_new_line : (string -> unit) -> bool -> int -> unit

(** Just a dummy function to make it easier to generate functions. *)
val dummy : unit -> unit

(** Prints the given terminal. 

	 @param os where to print the gviz information.
	 @param do_print if true then print newlines otherwise do nothing.
	 @param terminal the terminal to be printed.
	 @return unit *)
val ppast_terminal : (string -> unit) -> bool -> Grammar_name_syntax.__terminal__ -> unit

(** A function that prints a textual representation of a syntax tree.

	 @param os where to print the information
	 @param do_print if true print newlines otherwise do nothing
	 @param cons a string of representing the previous type name.
	 @param x of type elementn. This will be used to print the gviz information for elementn. 
	 @return unit *)
val ppast_element0 : (string -> unit) -> bool -> Grammar_name_syntax.element0 -> unit

  ...

(** A function that prints a textual representation of a syntax tree.

	 @param os where to print the information
	 @param do_print if true print newlines otherwise do nothing
	 @param cons a string of representing the previous type name.
	 @param x of type elementn. This will be used to print the gviz information for elementn. 
	 @return unit *)
val ppast_elementn : (string -> unit) -> bool -> Grammar_name_syntax.elementn -> unit

(** A wrapper function that will call the gviz function for the start 
	 of the grammar. 

	 @param os where to print the gviz information. 
	 @param do_print initially false. 
	 @param cons it is initially empty. 
	 @param x the start symbol of the grammar. 
	 @return unit *)
val ppast : (string -> unit) -> bool -> Grammar_name_syntax.grammar -> unit
