ocamlc -c util.ml
ocamlc -c trie.mli
ocamlc -c trie.ml
ocamlc -c grammar.mli
ocamlc -c grammar.ml
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
ocamlc -c dump_syntax_mode.ml
ocamlc -c dump_gviz.mli
ocamlc -c dump_gviz.ml
ocamlc -c main.ml
ocamlc -o gt.exe util.cmo usage.cmo trie.cmo version.cmo grammar.cmo parse.cmo lex.cmo dump.cmo dump_gviz.cmo dump_syntax_mode.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo main.cmo