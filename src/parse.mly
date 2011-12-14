%{
	let parse_error s = 
		let error = s^" on line "^(string_of_int (!Util.line))^
			" in file "^Util.fname 
		in 
		failwith error;;
%}
%token EOF BAR ARROW NEWLINE COLON EQUALS END_QUOTED LEFT RIGHT GTE LSQR_BRACKET RSQR_BRACKET LCURLY RCURLY LPAREN RPAREN COMMA TIMES PLUS LINECOMMENT CHAR KEYWORDS COMMENT NOTE INTEGER FLOAT DEC STRING KEYW TYPE SPECHAR BOOL ERR META FILEEXT NEWLINESENS DOT IMPORT STARTSYM
%token <string> ID QUOTED IQUOTED NONNEG
%start grammar
%type <Grammaru.Grammaru.grammaru> grammar
%type <Grammaru.Grammaru.productionu> production 
%type <Grammaru.Grammaru.productionu list * Grammaru.Grammaru.lexclassu list> pls
%type <Grammaru.Grammaru.elementu list> elements 
%type <Grammaru.Grammaru.elementu> element
%type <Grammaru.Grammaru.symbolu> symbol
%type <Grammaru.Grammaru.lexclassu list> lexclasses
%type <Grammaru.Grammaru.lexclassu> lexclass
%type <string * bool> quoted
%type <unit> end_quoted
%type <string> lr  
%%

grammar:
| imports ID linecmt pls highlights { 
  let imps = match $1 with [] -> None | a -> Some a in
  let (ps,lcs) = $4 in 
  Grammaru.Grammaru.Grammaru(imps,String.lowercase($2),$3,ps,lcs,$5) 
}

imports:
| { [] }
| IMPORT ID DOT imports { $2::$4 }

linecmt:
| { None }
| LINECOMMENT EQUALS IQUOTED DOT { Some($3) }

pls:
| production lexclasses { ([$1],$2) }
| production pls { 
  let (ps,lcs) = $2 in
  ($1::ps,lcs) 
}

production:
| ID COLON ID ARROW prod_body DOT  {
  let prod_or = match snd $5 with [] -> None | a -> Some(a) in
  Grammaru.Grammaru.Productionu($1,$3,fst $5,prod_or) 
}
| ID COLON ID ARROW DOT  {
  Grammaru.Grammaru.Productionu($1,$3,[],None) 
}

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
| symbol { Grammaru.Grammaru.Esymbolu($1) }
| LSQR_BRACKET prod_body RSQR_BRACKET { 
  let prods = (fst $2)::(snd $2) in
  Grammaru.Grammaru.Eoptionu(prods) 
}
| LCURLY prod_body RCURLY LPAREN lr COMMA GTE NONNEG RPAREN { 
    let prods = (fst $2)::(snd $2) in
	 Grammaru.Grammaru.Erepetitionu(prods,$5,$8) 
}
| LCURLY prod_body RCURLY PLUS { 
  let prods = (fst $2)::(snd $2) in
  Grammaru.Grammaru.Erepetitionu(prods,"right","1") 
}
| LCURLY prod_body RCURLY TIMES  { 
    let prods = (fst $2)::(snd $2) in
  Grammaru.Grammaru.Erepetitionu(prods,"right","0") 
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




highlights:
| EOF {"",[]}
| FILEEXT EQUALS IQUOTED KEYWORDS EQUALS keywords { $3,$6 }

keywords:
| keyword { $1::[] }
| keyword keywords { $1::$2}

keyword:
| LPAREN IQUOTED qts RPAREN COLON tp { ($6,$2::$3) }

qts:
| { [] } 
| COMMA IQUOTED qts { $2::$3 }

tp:
| COMMENT { "comment" }
| NOTE { "note" }
| INTEGER { "integer" }
| FLOAT { "float" }
| DEC { "dec" }
| STRING { "string" }
| KEYW { "keyword" }
| TYPE { "type" }
| SPECHAR { "special" }
| BOOL { "bool" }
| ERR { "error" }
| META { "meta" }
