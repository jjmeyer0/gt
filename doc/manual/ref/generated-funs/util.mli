(** The current file name of the file being parsed. *)
val fname : string ref

(** Keeps track of the current line for error reporting when parsing a file.*)
val line : int ref

(** The type declaration for position data. In this pair int represents the
	 position of the information and the string represents the file name of
	 the file being parsed.  *)
type pd = int * string

(** A function that takes a parameter of type pd and returns a string
	 that says 

	 @param pd 
	 @return str a string that says " on line <fst pd> in file <snd pd>" *)
val string_of_pos : pd -> string

(** This functions returns the current positions data when parsing a file. 

	 @param unit 
	 @return pd the current position data. *)
val cur_pd : unit -> pd
