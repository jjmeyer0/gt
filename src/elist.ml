module List = struct
  exception Does_not_exist

  include List

  let rev (l:'a list) : 'a list = 
    let rec rev (l:'a list) (acc:'a list) : 'a list =
      match l with
	| [] -> acc
	| hd::tl -> rev tl (hd::acc)
    in rev l []
  ;;

  let unique (l:'a list) : 'a list =
    let unique_set = Hashtbl.create (List.length l) in
    List.iter (fun x ->
      Hashtbl.replace unique_set x () 
    ) l;
    Hashtbl.fold (fun x () xs -> x :: xs) unique_set []
  ;;

  let repetitions (n:int) (ss:'a list) : 'a list = 
    let rec repetitions (n:int) (ss':'a list list) : 'a list =
      if n > 0  then repetitions (n-1) (ss::ss')
      else List.flatten ss'
    in
    if n <> 0 then repetitions n []
    else []
  ;;
  
  let position (needle:'a) (haystack:'a list) : int = 
    let rec position (i:int) (haystack:'a list) : int = 
      match haystack with
	| [] -> raise Does_not_exist
	| hd::tl when needle = hd -> i
	| hd::tl -> position (i+1) tl
    in position 0 haystack
  ;;

  let zip_with (l:'a list) (l':'b list) : ('a*'b) list =
    let rec zip_with (l:'a list) (l':'b list) (acc:('a*'b) list) : ('a*'b) list =
      match (l,l') with
	| ([],_) | (_,[]) -> rev acc
	| ([hd],hd'::tl') -> rev ((hd,hd')::acc)
	| (hd::tl,[hd']) -> rev ((hd,hd')::acc)
	| (hd::tl,hd'::tl') -> zip_with tl tl' ((hd,hd')::acc)
    in zip_with l l' []
  ;;

  let unzip (l:('a*'b) list) : 'a list * 'b list =
    let rec unzip (l:('a*'b) list) (a:'a list) (b:'b list) : 'a list * 'b list = 
      match l with
	| [] -> rev a,rev b
	| (l,r)::tl -> unzip tl (l::a) (r::b) 
    in unzip l [] []
  ;;

  let number (sl:'a list) : ('a*'int) list = 
    let len : int = List.length sl in
    let rec mk_list (i:int) (l:int list) : int list =
      if i < len then mk_list (i+1) (i::l)
      else i::l
    in zip_with sl (mk_list 0 [])
  ;;

end

