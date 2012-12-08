open Grammaru

let grammar_file = ref "";;
let custom_constructors = ref false;;

let usage = "usage: " ^ Sys.argv.(0) ^ " [-c] [-f string]"

let specs = [
  ("-c", Arg.Unit (fun () -> custom_constructors := true), ": Represents whether custom constructors will be asked for when converting EBNF to BNF.");
  ("-f", Arg.String (fun s -> grammar_file := s), ": The file name representing the wanted grammar.");
]

let () =
  Arg.parse specs (fun x -> raise (Arg.Bad ("Bad argument: " ^ x))) usage;
  let in_channel = if !grammar_file <> "" then open_in !grammar_file else stdin in
  let lexbuf = Lexing.from_channel in_channel in
  let parsed = Parse.grammar Lex.token lexbuf in

  

  Grammaru.prettyprint (Grammaru.to_bnf parsed);
  Printf.printf "\n\ndone\n";
  
  close_in in_channel
;;

