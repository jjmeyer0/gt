gt: trie.cmi grammar.cmi grammar.cmo usage.cmo version.cmo parse.cmo lex.cmo dump.cmi dump.cmo dump_gviz.cmi dump_gviz.cmo dump_syntax_mode.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo main.cmo Makefile
	ocamlc -o gt util.cmo usage.cmo trie.cmo version.cmo grammar.cmo parse.cmo lex.cmo dump.cmo dump_gviz.cmo dump_syntax_mode.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo main.cmo

emitted:
	$(MAKE) -f most_recent_emitted_Makefile

main.cmo: main.ml trie.cmi dump_gviz.cmi usage.cmo version.cmo parse.cmi lex.cmo grammar.cmi grammar.cmo dump_syntax_mode.cmi dump_syntax_mode.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo dump_gviz.cmo dump.cmo
	ocamlc -c main.ml

usage.cmo: usage.ml
	ocamlc -c usage.ml

version.cmo: version.ml
	ocamlc -c version.ml

util.cmo: util.ml
	ocamlc -c util.ml

dump.cmi: dump.mli trie.cmo grammar.cmo
	ocamlc -c dump.mli

dump.cmo: dump.ml trie.cmo grammar.cmo
	ocamlc -c dump.ml

dump_gviz.cmi: dump_gviz.mli trie.cmo grammar.cmo
	ocamlc -c dump_gviz.mli

dump_gviz.cmo: dump_gviz.ml trie.cmo grammar.cmo
	ocamlc -c dump_gviz.ml

dump_mlcf_ppast.cmo: dump_mlcf_ppast.ml trie.cmo grammar.cmo
	ocamlc -c dump_mlcf_ppast.ml

dump_mlcf_pp.cmo: dump_mlcf_pp.ml trie.cmo grammar.cmo
	ocamlc -c dump_mlcf_pp.ml

dump_mlcf_eq.cmo: dump_mlcf_eq.ml trie.cmo grammar.cmo
	ocamlc -c dump_mlcf_eq.ml

dump_syntax_mode.cmi: dump_syntax_mode.mli trie.cmo grammar.cmo
	ocamlc -c dump_syntax_mode.mli

dump_syntax_mode.cmo: dump_syntax_mode.ml trie.cmo grammar.cmo
	ocamlc -c dump_syntax_mode.ml

grammar.cmi: grammar.mli trie.cmo
	ocamlc -c grammar.mli

grammar.cmo: grammar.ml trie.cmo
	ocamlc -c grammar.ml

trie.cmi: trie.mli
	ocamlc -c trie.mli

trie.cmo: trie.ml
	ocamlc -c trie.ml

lex.cmo: lex.ml util.cmo parse.cmi
	ocamlc -c lex.ml

parse.cmo: parse.ml util.cmo grammar.cmo parse.cmi
	ocamlc -c parse.ml

parse.cmi: parse.mli grammar.cmo
	ocamlc -c parse.mli

parse.mli parse.ml: grammar.cmo util.cmo parse.mly
	ocamlyacc -v parse.mly

lex.ml: lex.mll parse.cmi util.cmo
	ocamllex lex.mll

.depend:
	rm -f .depend
	ocamldep *.ml* > .depend

clean:
	rm -f .depend gt lex.ml parse.mli parse.output parse.ml *.cm[iox]

include .depend
