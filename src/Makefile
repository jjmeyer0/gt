gt: estring.cmi elist.cmi trie.cmi grammar.cmi grammar.cmo grammare.cmi grammare.cmo grammaru.cmi grammaru.cmo usage.cmo version.cmo lex.cmo parse.cmo dump.cmi dump.cmo dump_gviz.cmi dump_gviz.cmo dump_syntax_mode.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo main.cmo Makefile
	ocamlc -o gt estring.cmo elist.cmo util.cmo usage.cmo trie.cmo version.cmo grammar.cmo grammare.cmo grammaru.cmo lex.cmo parse.cmo dump.cmo dump_gviz.cmo dump_syntax_mode.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo main.cmo

main.cmo: main.ml estring.cmi estring.cmo elist.cmi elist.cmo trie.cmi grammar.cmi grammar.cmo grammare.cmi grammare.cmo grammaru.cmi grammaru.cmo dump_gviz.cmi usage.cmo version.cmo parse.cmi lex.cmo dump_syntax_mode.cmo dump_mlcf_ppast.cmo dump_mlcf_pp.cmo dump_mlcf_eq.cmo dump.cmo
	 ocamlc -c main.ml

dump_mlcf_ppast.cmo: dump_mlcf_ppast.ml trie.cmo grammar.cmo
	ocamlc -c dump_mlcf_ppast.ml

dump_mlcf_pp.cmo: dump_mlcf_pp.ml trie.cmo grammar.cmo
	ocamlc -c dump_mlcf_pp.ml

dump_mlcf_eq.cmo: dump_mlcf_eq.ml trie.cmo grammar.cmo
	ocamlc -c dump_mlcf_eq.ml

emitted:
	$(MAKE) -f most_recent_emitted_Makefile

usage.cmo: usage.ml
	ocamlc -c usage.ml

version.cmo: version.ml
	ocamlc -c version.ml

util.cmo: util.ml
	ocamlc -c util.ml

dump_gviz.cmi: dump_gviz.mli trie.cmo grammar.cmo
	ocamlc -c dump_gviz.mli

dump_gviz.cmo: dump_gviz.ml trie.cmo grammar.cmo
	ocamlc -c dump_gviz.ml

dump.cmi: dump.mli trie.cmo grammar.cmo
	ocamlc -c dump.mli

dump.cmo: dump.ml trie.cmo grammar.cmo
	ocamlc -c dump.ml

dump_syntax_mode.cmo: dump_syntax_mode.ml trie.cmo grammar.cmo
	ocamlc -c dump_syntax_mode.ml

grammar.cmi: grammar.mli elist.cmi estring.cmi trie.cmo
	ocamlc -c grammar.mli

grammar.cmo: grammar.ml elist.cmi estring.cmi trie.cmo
	ocamlc -c grammar.ml

grammare.cmi: grammare.mli elist.cmi estring.cmi trie.cmo grammar.cmi
	ocamlc -c grammare.mli

grammare.cmo: grammare.ml elist.cmi estring.cmi trie.cmo grammar.cmi
	ocamlc -c grammare.ml

grammaru.cmi: grammaru.mli elist.cmi estring.cmi trie.cmo grammare.cmi
	ocamlc -c grammaru.mli

grammaru.cmo: grammaru.ml elist.cmi estring.cmi trie.cmo grammare.cmi
	ocamlc -c grammaru.ml

trie.cmi: trie.mli
	ocamlc -c trie.mli

trie.cmo: trie.ml
	ocamlc -c trie.ml

lex.cmo: lex.ml util.cmo parse.cmi
	ocamlc -c lex.ml

parse.cmo: parse.ml util.cmo grammaru.cmi grammaru.cmo parse.cmi
	ocamlc -c parse.ml

parse.cmi: parse.mli grammaru.cmi grammaru.cmo
	ocamlc -c parse.mli

parse.mli parse.ml: grammaru.cmi grammaru.cmo util.cmo parse.mly
	ocamlyacc -v parse.mly

lex.ml: lex.mll parse.cmi util.cmo
	ocamllex lex.mll

elist.cmi: elist.mli
	ocamlc -c elist.mli

elist.cmo: elist.ml
	ocamlc -c elist.ml

estring.cmi: estring.mli
	ocamlc -c estring.mli

estring.cmo: estring.ml
	ocamlc -c estring.ml

.depend:
	rm -f .depend
	ocamldep *.ml* > .depend

clean:
	rm -f .depend gt lex.ml parse.mli parse.output parse.ml *.cm[iox]

include .depend
