(** The type definitions for ther terminals of the defined language. *)
type token =
  | EOF
  | TERMINAL_NOT_IN_AST0 of (Lists_syntax.__term_not_in_ast__)

		...

  | TERMINAL_NOT_IN_ASTN of (Lists_syntax.__term_not_in_ast__)
  | TERMINAL_IN_AST0 of (Lists_syntax.__terminal__)

		...

  | TERMINAL_IN_ASTN of (Lists_syntax.__terminal__)

val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Lists_syntax.grammar option
