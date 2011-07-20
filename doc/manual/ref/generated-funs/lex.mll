{open Grammar_name_parse;;}

rule token = parse
  | ['\t' ' ' ]+ { token lexbuf }
  | '#'  (_ # ['\n' '\r'])* { token lexbuf }
  | ['\n' '\r']+ as str { Grammar_name_util.line := (!Grammar_name_util.line + (String.length str)); token lexbuf }
  | string_lit0 { NONASTTERMINAL0(Grammar_name_util.cur_pd()) }
	...
  | string_litn { NONASTTERMINALN(Grammar_name_util.cur_pd()) }
  | regexp0 as str { ASTTERMINAL0((Grammar_name_util.cur_pd(),str)) }
	...
  |  regexn as str { ASTTERMINALN(Grammar_name_util.cur_pd(),str) }

  | eof { EOF }
  | _ {failwith((Lexing.lexeme lexbuf)^": lexing error"^(Grammar_name_util.string_of_pos (Grammar_name_util.cur_pd())))}{}
