VERSION=1.0

all:
	cd src; $(MAKE)

clean:
	cd src; $(MAKE) clean

update:
	cd mlcf; $(MAKE); cd ../src; $(MAKE);

distro:
	cd src; $(MAKE) clean; cd ..; \
	mkdir ../gtr/gt; \
	cp compile.bat ../gtr/gt/; \
	cp Makefile ../gtr/gt/; \
	cp README ../gtr/gt/README; \
	mkdir ../gtr/gt/doc; \
	mkdir ../gtr/gt/doc/manual; \
	cp ../gtr/doc/manual/main.pdf ../gtr/gt/doc/manual/main.pdf; \
	cp ../gtr/doc/manual/main.tex ../gtr/gt/doc/manual/main.tex; \
	cp ../gtr/doc/manual/main.toc ../gtr/gt/doc/manual/main.toc; \
	cp ../gtr/doc/manual/Makefile ../gtr/gt/doc/manual/Makefile; \
	mkdir ../gtr/gt/doc/manual/examples; \
	cp ../gtr/doc/manual/examples/examples.tex ../gtr/gt/doc/manual/examples/examples.tex; \
	mkdir ../gtr/gt/doc/manual/examples/bnf; \
	cp ../gtr/doc/manual/examples/bnf/bnf.tex ../gtr/gt/doc/manual/examples/bnf/bnf.tex; \
	mkdir ../gtr/gt/doc/manual/examples/bnf/simple; \
	cp ../gtr/doc/manual/examples/bnf/simple/genfunctions.tex ../gtr/gt/doc/manual/examples/bnf/simple/genfunctions.tex; \
	cp ../gtr/doc/manual/examples/bnf/simple/simple_eq.ml ../gtr/gt/doc/manual/examples/bnf/simple/simple_eq.ml; \
	cp ../gtr/doc/manual/examples/bnf/simple/simple_gviz.ml ../gtr/gt/doc/manual/examples/bnf/simple/simple_gviz.ml; \
	cp ../gtr/doc/manual/examples/bnf/simple/simple_pp.ml ../gtr/gt/doc/manual/examples/bnf/simple/simple_pp.ml; \
	cp ../gtr/doc/manual/examples/bnf/simple/simple.dot ../gtr/gt/doc/manual/examples/bnf/simple/simple.dot; \
	cp ../gtr/doc/manual/examples/bnf/simple/simple.png ../gtr/gt/doc/manual/examples/bnf/simple/simple.png; \
	cp ../gtr/doc/manual/examples/bnf/simple/simple.tex ../gtr/gt/doc/manual/examples/bnf/simple/simple.tex; \
	cp ../gtr/doc/manual/examples/bnf/simple/syntrees.tex ../gtr/gt/doc/manual/examples/bnf/simple/syntrees.tex; \
	mkdir ../gtr/gt/doc/manual/examples/ebnf; \
	cp ../gtr/doc/manual/examples/ebnf/ebnf.tex ../gtr/gt/doc/manual/examples/ebnf/ebnf.tex; \
	mkdir ../gtr/gt/doc/manual/examples/ebnf/arith; \
	cp ../gtr/doc/manual/examples/ebnf/arith/arith_syntax.ml ../gtr/gt/doc/manual/examples/ebnf/arith/arith_syntax.ml; \
	cp ../gtr/doc/manual/examples/ebnf/arith/arith_util.ml ../gtr/gt/doc/manual/examples/ebnf/arith/arith_util.ml; \
	cp ../gtr/doc/manual/examples/ebnf/arith/arith.tex ../gtr/gt/doc/manual/examples/ebnf/arith/arith.tex; \
	cp ../gtr/doc/manual/examples/ebnf/arith/ebnf-arith.dot ../gtr/gt/doc/manual/examples/ebnf/arith/ebnf-arith.dot; \
	cp ../gtr/doc/manual/examples/ebnf/arith/ebnf-arith.png ../gtr/gt/doc/manual/examples/ebnf/arith/ebnf-arith.png; \
	cp ../gtr/doc/manual/examples/ebnf/arith/genfunctions.tex ../gtr/gt/doc/manual/examples/ebnf/arith/genfunctions.tex; \
	cp ../gtr/doc/manual/examples/ebnf/arith/syntrees.tex ../gtr/gt/doc/manual/examples/ebnf/arith/syntrees.tex; \
	mkdir ../gtr/gt/doc/manual/examples/ebnf/simple; \
	cp ../gtr/doc/manual/examples/ebnf/simple/ebnf-simple.dot ../gtr/gt/doc/manual/examples/ebnf/simple/ebnf-simple.dot; \
	cp ../gtr/doc/manual/examples/ebnf/simple/ebnf-simple.png ../gtr/gt/doc/manual/examples/ebnf/simple/ebnf-simple.png; \
	cp ../gtr/doc/manual/examples/ebnf/simple/expr_lex.mll ../gtr/gt/doc/manual/examples/ebnf/simple/expr_lex.mll; \
	cp ../gtr/doc/manual/examples/ebnf/simple/expr_parse.mly ../gtr/gt/doc/manual/examples/ebnf/simple/expr_parse.mly; \
	cp ../gtr/doc/manual/examples/ebnf/simple/genfunctions.tex ../gtr/gt/doc/manual/examples/ebnf/simple/genfunctions.tex; \
	cp ../gtr/doc/manual/examples/ebnf/simple/syntrees.tex ../gtr/gt/doc/manual/examples/ebnf/simple/syntrees.tex; \
	cp ../gtr/doc/manual/examples/ebnf/simple/simple.tex ../gtr/gt/doc/manual/examples/ebnf/simple/simple.tex; \
	mkdir ../gtr/gt/doc/manual/intro; \
	cp ../gtr/doc/manual/intro/install.tex ../gtr/gt/doc/manual/intro/install.tex; \
	cp ../gtr/doc/manual/intro/introduction.tex ../gtr/gt/doc/manual/intro/introduction.tex; \
	cp ../gtr/doc/manual/intro/tutorial.tex ../gtr/gt/doc/manual/intro/tutorial.tex; \
	mkdir ../gtr/gt/doc/manual/ref; \
	cp ../gtr/doc/manual/ref/gtref.tex ../gtr/gt/doc/manual/ref/gtref.tex; \
	cp ../gtr/doc/manual/ref/mlcfref.tex ../gtr/gt/doc/manual/ref/mlcfref.tex; \
	cp ../gtr/doc/manual/ref/output-files.tex ../gtr/gt/doc/manual/ref/output-files.tex; \
	mkdir ../gtr/gt/doc/manual/ref/generated-funs; \
	cp ../gtr/doc/manual/ref/generated-funs/eq.ml ../gtr/gt/doc/manual/ref/generated-funs/eq.ml; \
	cp ../gtr/doc/manual/ref/generated-funs/gviz.ml ../gtr/gt/doc/manual/ref/generated-funs/gviz.ml; \
	cp ../gtr/doc/manual/ref/generated-funs/lex.mll ../gtr/gt/doc/manual/ref/generated-funs/lex.mll; \
	cp ../gtr/doc/manual/ref/generated-funs/parse.mly ../gtr/gt/doc/manual/ref/generated-funs/parse.mly; \
	cp ../gtr/doc/manual/ref/generated-funs/pp.ml ../gtr/gt/doc/manual/ref/generated-funs/pp.ml; \
	cp ../gtr/doc/manual/ref/generated-funs/syntax.ml ../gtr/gt/doc/manual/ref/generated-funs/syntax.ml; \
	cp ../gtr/doc/manual/ref/generated-funs/util.ml ../gtr/gt/doc/manual/ref/generated-funs/util.ml; \
	mkdir ../gtr/gt/mlcf; \
	cp ../gtr/mlcf/Makefile ../gtr/gt/mlcf/Makefile; \
	cp ../gtr/mlcf/mlcf.gra ../gtr/gt/mlcf/mlcf.gra; \
	mkdir ../gtr/gt/mlcf/functions; \
	cp ../gtr/mlcf/functions/dump_eq.mlcf ../gtr/gt/mlcf/functions/dump_eq.mlcf; \
	cp ../gtr/mlcf/functions/dump_pp.mlcf ../gtr/gt/mlcf/functions/dump_pp.mlcf; \
	cp ../gtr/mlcf/functions/dump_ppast.mlcf ../gtr/gt/mlcf/functions/dump_ppast.mlcf; \
	mkdir ../gtr/gt/mlcf/src; \
	cp ../gtr/mlcf/src/dump_gt_files.ml ../gtr/gt/mlcf/src/dump_gt_files.ml;\
	cp ../gtr/mlcf/src/dump_ocaml.ml ../gtr/gt/mlcf/src/dump_ocaml.ml;\
	cp ../gtr/mlcf/src/dump.ml ../gtr/gt/mlcf/src/dump.ml;\
	cp ../gtr/mlcf/src/grammar.ml ../gtr/gt/mlcf/src/grammar.ml;\
	cp ../gtr/mlcf/src/lex.mll ../gtr/gt/mlcf/src/lex.mll;\
	cp ../gtr/mlcf/src/main.ml ../gtr/gt/mlcf/src/main.ml;\
	cp ../gtr/mlcf/src/Makefile ../gtr/gt/mlcf/src/Makefile;\
	cp ../gtr/mlcf/src/mlcf_main.ml ../gtr/gt/mlcf/src/mlcf_main.ml;\
	cp ../gtr/mlcf/src/parse.mly ../gtr/gt/mlcf/src/parse.mly;\
	cp ../gtr/mlcf/src/trie.ml ../gtr/gt/mlcf/src/trie.ml;\
	cp ../gtr/mlcf/src/util.ml ../gtr/gt/mlcf/src/util.ml;\
	mkdir ../gtr/gt/src; \
	cp ../gtr/src/compile.bat ../gtr/gt/src/compile.bat;\
	cp ../gtr/src/dump_gviz.ml ../gtr/gt/src/dump_gviz.ml;\
	cp ../gtr/src/dump_gviz.mli ../gtr/gt/src/dump_gviz.mli;\
	cp ../gtr/src/dump_mlcf_eq.ml ../gtr/gt/src/dump_mlcf_eq.ml;\
	cp ../gtr/src/dump_mlcf_pp.ml ../gtr/gt/src/dump_mlcf_pp.ml;\
	cp ../gtr/src/dump_mlcf_ppast.ml ../gtr/gt/src/dump_mlcf_ppast.ml;\
	cp ../gtr/src/dump_syntax_mode.mli ../gtr/gt/src/dump_syntax_mode.mli;\
	cp ../gtr/src/dump_syntax_mode.ml ../gtr/gt/src/dump_syntax_mode.ml;\
	cp ../gtr/src/dump.ml ../gtr/gt/src/dump.ml;\
	cp ../gtr/src/dump.mli ../gtr/gt/src/dump.mli;\
	cp ../gtr/src/grammar.ml ../gtr/gt/src/grammar.ml;\
	cp ../gtr/src/grammar.mli ../gtr/gt/src/grammar.mli;\
	cp ../gtr/src/lex.mll ../gtr/gt/src/lex.mll;\
	cp ../gtr/src/main.ml ../gtr/gt/src/main.ml;\
	cp ../gtr/src/Makefile ../gtr/gt/src/Makefile;\
	cp ../gtr/src/parse.mly ../gtr/gt/src/parse.mly;\
	cp ../gtr/src/trie.ml ../gtr/gt/src/trie.ml;\
	cp ../gtr/src/trie.mli ../gtr/gt/src/trie.mli;\
	cp ../gtr/src/usage.ml ../gtr/gt/src/usage.ml;\
	cp ../gtr/src/util.ml ../gtr/gt/src/util.ml;\
	cp ../gtr/src/version.ml  ../gtr/gt/src/version.ml;\
	mkdir ../gtr/gt/syntax_highlighters; \
	cp ../gtr/syntax_highlighters/gra.lang ../gtr/gt/syntax_highlighters/gra.lang; \
	cp ../gtr/syntax_highlighters/gtril.lang ../gtr/gt/syntax_highlighters/gtril.lang; \
	cp ../gtr/syntax_highlighters/mlcf.lang ../gtr/gt/syntax_highlighters/mlcf.lang; \
	cp ../gtr/syntax_highlighters/readme ../gtr/gt/syntax_highlighters/readme; \
	mkdir ../gtr/gt/tests; \
	cp ../gtr/tests/ebnf-example.gra ../gtr/gt/tests/ebnf-example.gra; \
	cp ../gtr/tests/ebnf-merged-exprs.gra ../gtr/gt/tests/ebnf-merged-exprs.gra; \
	cp ../gtr/tests/ebnf-simple-exprs.gra ../gtr/gt/tests/ebnf-simple-exprs.gra; \
	cp ../gtr/tests/ops.gra ../gtr/gt/tests/ops.gra; \
	cp ../gtr/tests/sample.gra ../gtr/gt/tests/sample.gra; \
	cp ../gtr/tests/simple.gra ../gtr/gt/tests/simple.gra; \
	cp ../gtr/tests/strat.gra ../gtr/gt/tests/strat.gra; \
	mkdir ../gtr/gt/tests/test-files; \
	cp ../gtr/tests/test-files/ebnf.e ../gtr/gt/tests/test-files/ebnf.e; \
	cp ../gtr/tests/test-files/ebnf.expr ../gtr/gt/tests/test-files/ebnf.expr; \
	cd ../gtr; tar cvfz grammar-tool.tgz gt/; \
	rm -R ../gtr/gt

# by default, it is not necessary to run mlcf, the tool which generated
# some of the files in src/