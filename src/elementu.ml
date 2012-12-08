module rec Esymbolu : sig
  type t = string
  val to_symbol : t -> Grammar.Symbol.t
  val to_string : t -> t
end = struct
  type t = string
  let to_symbol sym = Grammar.Symbol.make sym true ;;
  let to_string s = s
end


and Eoptionu : sig
  type t = Elementu.t list list
  val to_symbol : t -> Grammar.Symbol.t list list
  val to_string : t -> string
end = struct
  type t = Elementu.t list list
  let to_symbol elements = [[]];;
  let to_string els = 
    let st = List.map (fun es -> List.map (Elementu.to_string) es) els in
    ("[ " ^ (List.fold_left (^) " | " (List.flatten st)) ^ " ]")
  ;;
end


and Erepetitionu : sig
  type assoc = Left | Right
  type reps = int
  type t = Elementu.t list list * assoc * reps
  val left : assoc
  val right : assoc
  val make : Elementu.t list list -> assoc -> reps -> t
  val erepu2element : t -> Grammar.Symbol.t list list
  val to_string : t -> string
end = struct
  type assoc = Left | Right
  type reps = int
  type t = Elementu.t list list * assoc * reps
  let left = Left
  let right = Right
  let make (e:Elementu.t list list) (a:assoc) (r:reps) : t = e,a,r
  let erepu2element (elements,s,s') = [[]];;
  let to_string ((els,assoc,reps) as element) =
    let st = List.map (fun es -> List.map (Elementu.to_string) es) els in
    let syms = List.fold_left (^) " | " (List.flatten st) in
    let get_assoc = function
      | Left -> "left" 
      | Right -> "right" 
    in
    let get = function
      | (els,Right,0) -> "{ " ^ syms ^ " }*"
      | (els,Right,1) -> "{ " ^ syms ^ " }+"
      | (els,assoc,n) -> 
	"{ " ^ syms ^ " }(" ^ (get_assoc assoc) ^ ",>=" ^ (string_of_int n) ^ ")"
    in get element
  ;;
end


and Elementu : sig
  type t =
    | Esymbolu of Esymbolu.t
    | Eoptionu of Eoptionu.t
    | Erepetitionu of Erepetitionu.t

  val make_esymbolu : Esymbolu.t -> t
  val make_erepu : Erepetitionu.t -> t
  val make_eoptu : Eoptionu.t -> t
  val to_string : t -> string
end = struct
  type t =
    | Esymbolu of Esymbolu.t
    | Eoptionu of Eoptionu.t
    | Erepetitionu of Erepetitionu.t

  let to_string = function
    | Esymbolu s -> Esymbolu.to_string s
    | Eoptionu e -> Eoptionu.to_string e
    | Erepetitionu e -> Erepetitionu.to_string e
      
  let make_esymbolu e = Esymbolu e
  let make_erepu e = Erepetitionu e
  let make_eoptu e = Eoptionu e
end
