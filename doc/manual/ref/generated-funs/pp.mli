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

(** A function to pretty print for element0. This function will print newlines in
	 the correct location for element0.

	 @param os where to print the information
	 @param do_print if true print newlines otherwise do nothing.
	 @param el The start symbol of the grammar
	 @return unit *)
val pp_element0 : (string -> unit) -> bool -> Grammar_name_syntax.element0 -> unit

  ...

(** A function to pretty print for elementn. This function will print newlines in
	 the correct location for elementn.

	 @param os where to print the information
	 @param do_print if true print newlines otherwise do nothing.
	 @param el The start symbol of the grammar
	 @return unit*)
val pp_elementn : (string -> unit) -> bool -> Grammar_name_syntax.elementn -> unit

(** The wrapper function of pp that will call the function for the start symbol.

	 @param os where to print the information
	 @param do_print if true print newlines otherwise do nothing.
	 @param el The start symbol of the grammar
	 @return unit *)
val pp : (string -> unit) -> bool -> Grammar_name_syntax.element0 -> unit
