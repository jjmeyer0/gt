all: grammar.cmo grammaru.cmo grammaru.cmo util.cmo lex.cmo parse.cmo main.cmo Makefile
	ocamlc -o gt grammar.cmo grammaru.cmo util.cmo lex.cmo parse.cmo main.cmo

main.cmo: grammar.cmo grammaru.cmo util.cmo lex.cmo parse.cmo
	ocamlc -c main.ml

grammar.cmo: grammar.cmo
	ocamlc -c grammar.ml

grammaru.cmo: grammaru.cmo
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

util.cmo: util.ml
	ocamlc -c util.ml

clean:
	rm -f .depend gt lex.ml parse.mli parse.output parse.ml *.cm[iox]
