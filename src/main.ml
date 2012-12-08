open Grammaru

let grammar_file_name = ref Sys.argv.(1)

let _ =
  let cin =
    if !grammar_file_name <> "" then open_in !grammar_file_name
    else stdin
  in
  let parsed =
    let lexbuf = Lexing.from_channel cin in
    Parse.grammar Lex.token lexbuf
  in

  Grammaru.prettyprint (Grammaru.to_bnf parsed);
  print_string "\n\ndone\n"
