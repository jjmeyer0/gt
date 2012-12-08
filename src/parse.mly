%{
  let parse_error s = 
    let error = s^" on line "^string_of_int (!Util.line)^" in file "^Util.fname in 
    failwith error
 ;;
%}
%token EOF BAR ARROW NEWLINE COLON EQUALS END_QUOTED LEFT RIGHT GTE LSQR_BRACKET RSQR_BRACKET LCURLY RCURLY LPAREN RPAREN COMMA TIMES PLUS LINECOMMENT CHAR DOT IMPORT
%token <string> ID QUOTED IQUOTED NONNEG
%start grammar
%type <Grammaru.Grammaru.t> grammar
%type <Grammaru.Productionu.t> production 
%type <Grammaru.Productionu.t list * Grammaru.LexClassu.t list> pls
%type <Grammaru.Elementu.t list> elements 
%type <Grammaru.Elementu.t> element
%type <Grammaru.Esymbolu.t> symbol
%type <Grammaru.LexClassu.t list> lexclasses
%type <Grammaru.LexClassu.t> lexclass
%type <string * bool> quoted
%type <unit> end_quoted
%type <string> lr  
%%

grammar:
| imports ID linecmt pls { 
  let imps = match $1 with 
    | [] -> None 
    | a -> Some a 
  in
  let (ps,lcs) = $4 in 
  let lower_id = String.lowercase $2 in
  Grammaru.Grammaru.make imps lower_id $3 ps lcs
}

imports:
| { [] }
| IMPORT ID DOT imports { $2::$4 }

linecmt:
| { None }
| LINECOMMENT EQUALS IQUOTED DOT { Some($3) }

pls:
| production lexclasses { ([$1],$2) }
| production pls { let (ps,lcs) = $2 in ($1::ps,lcs) }
| EOF { [],[] }

production:
| ID COLON ID ARROW prod_body DOT  {
  let prod_or = match snd $5 with 
    | [] -> None 
    | a -> Some a 
  in Grammaru.Productionu.make $1 $3 (fst $5) prod_or
}
| ID COLON ID ARROW DOT  { Grammaru.Productionu.make $1 $3 [] None }

prod_body:
| BAR elements prod_ors { $2,$3 }
| elements prod_ors { $1,$2 }

prod_ors:
| { [] }
| BAR elements prod_ors { $2::$3  }

symbol:
| ID { $1 }

elements :
| element { $1::[] }
| element elements { $1::$2 }

element :
| symbol { Grammaru.Elementu.make_esymbolu $1 }
| LSQR_BRACKET prod_body RSQR_BRACKET { 
  let prods = (fst $2)::(snd $2) in
  Grammaru.Elementu.make_eoptu prods
}
| LCURLY prod_body RCURLY LPAREN lr COMMA GTE NONNEG RPAREN { 
  let prods = (fst $2)::(snd $2) in
  let rep = int_of_string $8 in
  let assoc = 
    if $5 = "right" then Grammaru.Erepetitionu.right 
    else Grammaru.Erepetitionu.left 
  in
  let el = Grammaru.Erepetitionu.make prods assoc rep in
  Grammaru.Elementu.make_erepu el
}
| LCURLY prod_body RCURLY PLUS { 
  let prods = (fst $2)::(snd $2) in
  let els = Grammaru.Erepetitionu.make prods Grammaru.Erepetitionu.right 1 in
  Grammaru.Elementu.make_erepu els
}
| LCURLY prod_body RCURLY TIMES  { 
  let prods = (fst $2)::(snd $2) in
  let erep = Grammaru.Erepetitionu.make prods Grammaru.Erepetitionu.right 0 in
  Grammaru.Elementu.make_erepu erep
}

lr :
| LEFT { "left" }
| RIGHT { "right" }

lexclasses:
| { [] }
| lexclass lexclasses { $1::$2 }

lexclass:
| ID EQUALS quoted DOT { ($1, $3) }

quoted:
| QUOTED end_quoted { ($1,true) }
| CHAR QUOTED end_quoted { ("char"^$2,true) }
| IQUOTED  { ($1,false) }

end_quoted:
| END_QUOTED {()}
