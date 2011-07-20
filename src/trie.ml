open Char;;

type 'a trie = Tnone 
             | Texact of char list * 'a
             | Tnext of 'a option * 'a trie array

let mk_trievec a = (Array.make 128 a);;

let rec trie_insert t s a = 
  match t with 
    Tnone -> Texact(s,a)
  | Texact(s',a') -> 
       (trie_insert (trie_insert (Tnext(None,(mk_trievec Tnone))) s' a') s a)
  | Tnext(o,v) ->
       match s with
         [] -> Tnext(Some(a),v)
       | c::s' -> 
           let cc = (code c) in
           Array.set v cc (trie_insert (Array.get v cc) s' a);
           Tnext(o,v)

let rec trie_lookup t s = 
   match t with
     Tnone -> None
   | Texact(s',a) -> if s = s' then Some(a) else None
   | Tnext(o,v) -> 
       match s with
         [] -> o
       | c::s' -> trie_lookup (Array.get v (code c)) s'

let rec charlist_of_string s i =
  if (i = String.length s) then
    []
  else
    (s.[i])::(charlist_of_string s (i+1));;

let charlist_of_string s = charlist_of_string s 0;;

let trie_insert t s a = trie_insert t (charlist_of_string s) a;;
let trie_lookup t s = trie_lookup t (charlist_of_string s);;
let trie_contains t s =
  match (trie_lookup t s) with
      Some(_) -> true
    | _ -> false;;


(*
let t = Tnone;;

let t = trie_insert t "cat" 3;;

let t = trie_insert t "call" 4;;

match (trie_lookup t "call") with
    Some x -> print_int x; print_string "\n"
  | None -> print_string "not found\n";;
*)
