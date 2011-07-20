all:
	cd src
	ocamlc -c util.ml
	ocamlc -c trie.ml
	ocamlc -c grammar.ml
	ocamlyacc -v parse.mly
	ocamlc -c parse.mli
	ocamlc -c parse.ml
	ocamllex lex.mll
	ocamlc -c lex.ml
	ocamlc -c dump.ml
	ocamlc -c dump_mlcf_ppast.ml
	ocamlc -c dump_mlcf_pp.ml
	ocamlc -c dump_mlcf_eq.ml
	ocamlc -c dump_gviz.ml
	ocamlc -c main.ml
	ocamlc -o gt.exe util.cmo trie.cmo grammar.cmo parse.cmo lex.cmo dump.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo dump_gviz.cmo main.cmo


REM add support for making and updating mlcf and making a distro.
