type 'a trie =
    Tnone
  | Texact of char list * 'a
  | Tnext of 'a option * 'a trie array
val mk_trievec : 'a -> 'a array
val charlist_of_string : string -> char list
val trie_insert : 'a trie -> string -> 'a -> 'a trie
val trie_lookup : 'a trie -> string -> 'a option
val trie_contains : 'a trie -> string -> bool
