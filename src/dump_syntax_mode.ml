open Grammar;;
open Trie;;

let special_chars : char list = 
  ['`';'~';'!';'@';'#';'$';'%';'^';'&';'*';'(';')';'-';
	'_';'=';'+';'[';'{';'}';']';'|';'\\';'\'';'"';':';';';
	',';'.';'>';'<';'/';'?'];;

let is_in_ast (t:unit trie) (s:string) : bool = trie_contains t s

let delim (cmt:string option) : string =
  match cmt with None -> "\"#\"" | Some(x) -> x

let get_comment_style (cmt:string option) : string = 
  let delim = String.sub (delim cmt) 1 (String.length (delim cmt) - 2) in
  let con = "        <context id=\"comment\" style-ref=\"comment\">\n          <start>" in
  let con' = delim^"</start>\n          <end>$</end>\n        </context>\n\n" in
  con^con'

let get_gedit (st:string) : string * string =
  match st with
	 | "comment" -> "Comment","comment"
	 | "note" -> "Doc comments","note"
	 | "integer" -> "Base-N Integer","base-n-integer"
	 | "flaot" -> "Floating Point number","floating-point"
	 | "dec" -> "Decimal number","decimal"
	 | "string" -> "String","string"
	 | "keyword" -> "Keyword","keyword"
	 | "type" -> "Data Type","type"
	 | "special" -> "Escaped Character","special-char"
	 | "bool" -> "Boolean value","boolean"
	 | "meta" -> "Type, module or object keyword","keyword"
	 | _ -> "Keyword","keyword"

let get_emacs (st:string) : string =
  match st with
	 | "comment" -> "font-lock-comment-face"
	 | "note" -> "font-lock-builtin-face"
	 | "integer" -> "font-lock-reference-face"
	 | "flaot" -> "font-lock-constant-face"
	 | "dec" -> "font-lock-constant-face"
	 | "string" -> "font-lock-string-face"
	 | "keyword" -> "font-lock-keyword-face"
	 | "type" -> "font-lock-type-face"
	 | "special" -> "font-lock-preprocessor-face"
	 | "bool" -> "font-lock-type-face"
	 | "meta" -> "font-lock-function-name-face"
	 | _ -> "font-lock-keyword-face"
(*
let get_vim (st:string) : string =
  match st with
	 | "comment" -> "Comment"
	 | "note" -> "Delimeter"
	 | "integer" -> "Number"
	 | "flaot" -> "Float"
	 | "dec" -> "Float"
	 | "string" -> "String"
	 | "keyword" -> "Keyword"
	 | "type" -> "Type"
	 | "special" -> "Special"
	 | "bool" -> "Boolean"
	 | "meta" -> "Function"
	 | _ -> "Keyword"
		*)

let dump_gedit (g:grammar) : unit = 
  match g with Grammar(name,_,cmt,_,_,_,syncolor) ->
	 let ofile = (open_out (name ^ ".lang")) in
	 let os = output_string ofile in
	 let ext = fst syncolor in
	 let keywords = snd syncolor in
	 let ext' s = String.sub s 1 (String.length s - 2) in 
	 os "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n";
	 os ("<language id=\""^name^"\" _name=\""^String.uppercase name^"\" version=\"2.0\" _section=\"Others\">\n");
	 os ("  <metadata>\n    <property name=\"mimetypes\">text/x-"^(ext' ext)^"</property>\n");
	 os ("    <property name=\"globs\">*."^(ext' ext)^"</property>\n  </metadata>\n\n  <styles>\n");

	 let i = ref 0 in
	 List.iter(fun (s,b) ->
		let pr = get_gedit s in incr i;
		os ("    <style id=\""^s);
		os ("\" _name=\""^fst pr^"\" map-to=\"def:"^snd pr^"\"/>\n")
	 ) keywords; 

	 os "    <style id=\"comment\" _name=\"Comment\" map-to=\"def:comment\"/>\n";
	 os "  </styles>\n\n  <definitions>\n    <context id="; 
	 os ("\""^name^"\">\n      <include>\n\n");
	 os (get_comment_style cmt);

	 List.iter(fun (tp,keys) ->
		incr i;
		let first = ref true in
		List.iter(fun s -> 
		  if(String.length s > 3) then (
			 if !first then ( 
				first := false;
				os ("        <context id=\""^tp^(string_of_int !i)^"\" style-ref=\"");
				os (tp^"\">\n"); incr i
			 );
			 os ("          <keyword>"^(ext' s)^"</keyword>\n")
		  )
		) keys;
		if not !first then os "        </context>\n\n";
		
		List.iter(fun s -> 
		  if(String.length s = 3) then (
			 os ("        <context id=\""^tp^(string_of_int !i)^"\" style-ref=\"");
			 os (tp^"\">\n");
			 os ("          <match>["^(ext' s)^"]</match>\n");
			 os "        </context>\n\n"; incr i
		  )
		) keys;		
	 ) keywords;

	 os "      </include>\n    </context>\n  </definitions>\n</language>";
	 close_out ofile
;;


(* Dump Emacs mode *)
let dump_emacs (g:grammar) : unit =
  match g with Grammar(name,_,cmt,_,_,_,syncolor) ->
	 let ofile = open_out (name ^ "-mode.el") in
	 let os = output_string ofile in
	 let keywords = snd syncolor in
	 let ext' e = String.sub e 1 (String.length e - 2) in 	 

	 List.iter(fun (tp,keys) ->
		os ("(defvar "^name^"-"^tp^"\n  '(");
		List.iter(fun k ->
		  if String.length k > 3 then (os k; os " ")
		) keys;
		os ")"; os ("\n  \""^name^" "^tp^".\")\n\n")
	 ) keywords;
	 
	 List.iter(fun (tp,keys) ->
		os ("(defvar "^name^"-"^tp^
				 "-regexp (regexp-opt "^name^"-"^tp^" 'words))\n");
	 ) keywords;

	 List.iter(fun (tp,keys) ->
		os ("(setq "^name^"-"^tp^" nil)\n");
	 ) keywords; 

	 
	 os ("(setq "^name^"-font-lock-keywords\n  `(\n");
	 List.iter(fun (tp,keys) ->
		let f = ref true in
		List.iter(fun k -> 
		  if String.length k = 3 then (
			 if !f then os ("  (\""^(ext' k)^"")
			 else os ("\\\\|"^(ext' k))
		  )
		) keys;
		if not !f then (os ("\" . "^(get_emacs tp)^")\n"); f := true);

		os ("   (,"^name^"-"^tp^"-regexp . "^(get_emacs tp)^")\n")
	 ) keywords;

	 os ("))\n\n(define-derived-mode "^name^"-mode fundamental-mode\n");
	 os ("  \""^name^" mode\"\n");
	 os ("  \"Major mode for editing "^name^"\"\n\n");
	 os ("  (setq font-lock-defaults '(("^name^"-font-lock-keywords)))\n\n");

	 List.iter(fun (tp,keys) -> 
		os ("  (setq "^name^"-"^tp^"-regexp nil)\n")
	 ) keywords;

	 os ("  (setq comment-start "^(delim cmt)^")\n  (setq comment-end \"\")\n\n");
	 os ("  (modify-syntax-entry ?"^(ext' (delim cmt))^" \"< b\" "^name^"-mode-syntax-table)\n");
	 os ("  (modify-syntax-entry ?\\n \"> b\" "^name^"-mode-syntax-table)\n)");;
		
(*
(* Dump vim mode *)
let dump_vim (g:grammar) : unit =
  match g with Grammar(name,_,cmt,_,_,_,syncolor) ->
	 let ofile = (open_out ("my"^name ^ "-mode.vim")) in
	 let os = output_string ofile in
	 let keywords = snd syncolor in
	 let ext' e = String.sub e 1 (String.length e - 2) in 
	 
	 os "if exists(\"b:current_syntax\")\n  finish\nendif\n\n";
	 
	 List.iter(fun (tp,keys) ->
		let f = ref true in
		os "syntax match "; os name; os tp; os "/\\s*\\(";
		List.iter(fun k ->
		  if !f then (os (ext' k); f:=false)
		  else os ("\\|"^(ext' k))
		) keys;
		os "\\)\\[/me=e-1 containedin="; os name; os tp; os " contained\n"
	 ) keywords;
	 
	 os "\n\n\n\n";
	 
	 List.iter(fun (tp,keys) ->
		os "highlight link "; os name; os tp; os (" "^get_vim tp^"\n")
	 ) keywords;
	 os ("let b:current_syntax = \""^name^"\"")
 *)	
		
let syncolor' (n:string) (t:unit trie) (lex:(string*string) list) : string*(string*string list) list = 
  let rec itr li =
	 match li with
		  [] -> []
		| (s,s')::tl ->
		  if String.length s' > 3 && not (is_in_ast t s) then 
			 (s'::(itr tl))
		  else itr tl
  in n,(("keyword",itr lex)::[])


let dump_syntax_mode (Grammar(n,l,c,prods,lex,t,syncolor):grammar) = 
  let g = Grammar(n,l,c,prods,lex,t,syncolor) in
  match syncolor with
	 | (ext,li) when ext <> "" && li <> [] -> dump_gedit g; dump_emacs g
	 | _ -> 
		let g = Grammar(n,l,c,prods,lex,t,syncolor' n t lex) in
		dump_gedit g; dump_emacs g
