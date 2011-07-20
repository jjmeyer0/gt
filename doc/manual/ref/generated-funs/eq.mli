(** A reference to keep track of the current line number. *)
val cur_line : int ref

(** Just a dummy function to make it easier to generate functions. *)
val dummy : unit -> unit

(** A function to compare to parameters of type start_sym. 

	 @param els a pair of 2 elements to check if they are equal.
	 @return b true if the are equal otherwise false *)
val eq_element0 : Grammar_name_syntax.start_sym * Grammar_name_syntax.start_sym -> bool

  ...

(** A function to compare to parameters of type elementn. 

	 @param els a pair of 2 elements to check if they are equal.
	 @return b true if the are equal otherwise false *)
val eq_elementn : Grammar_name_syntax.elementn * Grammar_name_syntax.elementn -> bool

(** A wrapper function that will call the eq function for the start 
	 of the grammar. 

	 @param els a pair of 2 elements to check if they are equal.
	 @return b true if the are equal otherwise false *)
val eq : Grammar_name_syntax.start_sym * Grammar_name_syntax.start_sym -> bool
