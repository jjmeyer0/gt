open Char
open Estring

module Trie = struct
  type 'a t = 
    | Tnone
    | Texact of char list * 'a
    | Tnext of 'a option * 'a t array

  let make a = Array.make 128 a;;

  let rec insert t s a = match t with
    | Tnone -> Texact(s,a)
    | Texact(s',a') -> insert (insert (Tnext (None, (make Tnone))) s' a') s a
    | Tnext (o,v) -> 
      match s with 
	| [] -> Tnext (Some a,v)
	| c::s' -> 
	  let cc = code c in
	  Array.set v cc (insert (Array.get v cc) s' a);
	  Tnext (o,v)
  ;;

  let lookup t s = match t with
    | Tnone -> None
    | Texact (s',a) when s = s' -> Some a
    | Texact (s',a) -> None
    | Tnext (o,v) -> 
      match s with
	| [] -> o
	| c::s' -> lookup (Array.get v (code c)) s'
  ;;

  let insert t s a = insert t (String.charlist_of_string s) a;;

  let lookup t s = lookup t (String.charlist_of_string s) a;;

  let contains t s = match (look_up t s) with
    | Some _ -> true
    | None -> false
end




(*
let t = Tnone;;

let t = trie_insert t "cat" 3;;

let t = trie_insert t "call" 4;;

match (trie_lookup t "call") with
    Some x -> print_int x; print_string "\n"
  | None -> print_string "not found\n";;
*)


(*let rec charlist_of_string s i =
  if (i = String.length s) then
    []
  else
    (s.[i])::(charlist_of_string s (i+1))
;;

let charlist_of_string s = charlist_of_string s 0;;*)
