module Grammaru :
  sig
    type highlights = string * (string * string list) list
    type symbolu = string
    type elementu =
        Esymbolu of symbolu
      | Eoptionu of elementu list list
      | Erepetitionu of elementu list list * string * string
    type productionu =
        Productionu of string * string * elementu list *
          elementu list list option
    type lexclassu = string * (string * bool)
    type grammaru =
        Grammaru of string list option * string * string option *
          productionu list * lexclassu list * highlights
    val get_nonterminal_strings : grammaru -> string list
    val get_start_symbol : grammaru -> string
    val is_newlnsens : string option -> bool
    val su2se : unit Trie.trie -> symbolu -> symbolu * bool
    val esu2ese :
      unit Trie.trie -> elementu list -> Grammare.Grammare.element list
    val esuopt2eseopt :
      unit Trie.trie ->
      elementu list list option -> Grammare.Grammare.element list list option
    val pul2pel :
      unit Trie.trie ->
      productionu list -> Grammare.Grammare.productione list
    val gu2ge : grammaru -> Grammare.Grammare.grammare
  end
