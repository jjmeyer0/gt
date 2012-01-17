%{
  open Grammar;;
  let parse_error s = 
    print_string s;
    print_string " on line ";
    print_int !Util.line;
	 print_string " in file ";
	 print_string !Util.file;
    print_string "\n";;

%}
%token EOF ARROW NEWLINE COLON EQUALS END_QUOTED LEFT RIGHT GTE LSQR_BRACKET RSQR_BRACKET LCURLY RCURLY LPAREN RPAREN COMMA TIMES PLUS LINECOMMENT CHAR
%token <string> ID QUOTED IQUOTED NONNEG
%start grammar
%type <Grammar.grammaru> grammar
%type <Grammar.productionu> production 
%type <Grammar.productionu list * Grammar.lexclassu list * string> pls
%type <Grammar.elementu list> elements 
%type <Grammar.elementu> element
%type <Grammar.symbolu> symbol
%type <Grammar.lexclassu list> lexclasses
%type <Grammar.lexclassu> lexclass
%type <string * bool> quoted
%type <unit> end_quoted end_iquoted 
%type <string> lr  
%%

grammar:
| ID NEWLINE LINECOMMENT EQUALS IQUOTED NEWLINE pls { let (ps,lcs,extradata) = $7 in
		     Grammaru(String.lowercase($1), Some($5), ps, lcs, extradata) }
| ID NEWLINE pls { let (ps,lcs,extradata) = $3 in
		     Grammaru(String.lowercase($1), None, ps, lcs, extradata) }

pls:
| production lexclasses extradata { ([$1],$2,$3) }
| production pls { let (ps,lcs,extradata) = $2 in
			     ($1::ps,lcs,extradata) }

production:
| ID COLON ID ARROW elements NEWLINE { Productionu($1,$3,$5) }

symbol:
| ID { $1 }

elements :
| { [] }
| element elements { $1::$2 }

element :
| symbol { Esymbolu($1) }
| LSQR_BRACKET elements RSQR_BRACKET { Eoptionu($2) }
| LCURLY elements RCURLY LPAREN lr COMMA GTE NONNEG RPAREN { Erepetitionu($2, $5, $8) }
| LCURLY elements RCURLY PLUS { Erepetitionu($2, "right", "1") }
| LCURLY elements RCURLY TIMES  { Erepetitionu($2, "right", "0") }

lr :
| LEFT { "left" }
| RIGHT { "right" }

lexclasses:
| lexclass { [$1] }
| lexclass lexclasses { $1::$2 }

lexclass:
| ID EQUALS quoted { ($1, $3) }

quoted:
| QUOTED end_quoted { ($1,true) }
| CHAR QUOTED end_quoted { ("char"^$2,true) }
| IQUOTED end_iquoted { ($1,false) }

end_quoted:
| END_QUOTED NEWLINE {()}
| END_QUOTED EOF {()}

end_iquoted:
| NEWLINE {()}
| EOF {()}

extradata : 
| QUOTED END_QUOTED { $1 }

extradata :
| { "" } 
