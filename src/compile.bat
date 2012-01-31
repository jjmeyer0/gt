	
ocamlc -c estring.mli
ocamlc -c estring.ml
ocamlc -c elist.mli
ocamlc -c elist.ml
ocamlc -c util.ml
	
ocamlc -c trie.mli

ocamlc -c trie.ml	
ocamlc -c version.ml
ocamlc -c usage.ml

ocamlc -c grammar.mli
ocamlc -c grammar.ml
ocamlc -c grammare.mli
ocamlc -c grammare.ml
ocamlc -c grammaru.mli
ocamlc -c grammaru.ml
	
ocamlyacc -v parse.mly
	
ocamlc -c parse.mli
	
ocamlc -c parse.ml
	
ocamllex lex.mll
	
ocamlc -c lex.ml
	
ocamlc -c dump.mli
ocamlc -c dump.ml
ocamlc -c dump_mlcf_ppast.ml
ocamlc -c dump_mlcf_pp.ml
ocamlc -c dump_mlcf_eq.ml
ocamlc -c dump_syntax_mode.mli
ocamlc -c dump_syntax_mode.ml
ocamlc -c dump_gviz.mli
ocamlc -c dump_gviz.ml
ocamlc -c main.ml

ocamlc -o gt.exe estring.cmo elist.cmo util.cmo usage.cmo trie.cmo version.cmo grammar.cmo grammare.cmo grammaru.cmo lex.cmo parse.cmo dump.cmo dump_gviz.cmo dump_syntax_mode.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo main.cmo
