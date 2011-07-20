(** A type used for terminals that are in the ast. *)
type __terminal__ = Grammar_name_util.pd * string

(** A type used for terminals that are not in the ast. *)
type __term_not_in_ast__ = Grammar_name_util.pd

(** Just a dummy type to make it easier to generate types. *)
type dummy = Dummy

(** The type definition for element0. This type is used when building
	 syntax trees. *)
and element0 = Production0 of Grammar_name_util.pd * el0 * ... * eln | ... | Productionn of Grammar_name_util.pd * el0 * ... * eln

  ...

(** The type definition for elementn. This type is used when building
	 syntax trees. *)
and elementn = Production0 of Grammar_name_util.pd * el0 * ... * eln | ... | Productionn of Grammar_name_util.pd * el0 * ... * eln


(** Just a dummy function to make it easier to generate functions. *)
val dummy : unit -> unit

(** A function to get the position data of a terminal that is in the ast.

	 @param terminal a terminal to get the position data. 
	 @return 'a the position data of a terminal in the ast. *)
val get_terminal_pd : 'a * 'b -> 'a

(** A function to get the position data of a terminal that is not in the ast.

	 @param terminal a terminal to get position data from
	 @return 'a position data of a terminal not in an ast. *)
val get_term_pd_not_in_ast : 'a -> 'a

(** A function to get the position data of an element with type element0.

	 @param el find the position data of element0. 
	 @return pd the positions data for el. *)
val pd_element0 : element0 -> Grammar_name_util.pd

  ...

(** A function to get the position data of an element with type element0.

	 @param el find the position data of element0. 
	 @return pd the positions data for el. *)
val pd_elementn : elementn -> Grammar_name_util.pd

(** A wrapper function that gets the position data of element0.

	 @param el find the position data of element0. 
	 @return pd the positions data for el. *)
val pd : element0 -> Grammar_name_util.pd
