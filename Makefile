INTERFACES 			= parse.mli
SOURCES    			= ast.ml parse.ml lex.ml eval.ml traducteur.ml main.ml
SOURCES_TESTS_VC    = ast.ml eval.ml testVerifsContext.ml
#GENERATED  		 = parse.ml parse.mli parse.automaton parse.conflicts
GENERATED  			= lex.ml parse.ml parse.mli parse.automaton parse.conflicts

compilateur: parse.mli $(SOURCES)
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o compilateur $(SOURCES)

testLex : parse.mli lex.ml testLex.ml ast.mli
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o testLex ast.ml parse.ml lex.ml testLex.ml

testsVerifsContext: parse.mli $(SOURCES)
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o testsVerifsContext $(SOURCES_TESTS_VC)

ast.mli: ast.ml
	ocamlc -c ast.ml

lex.ml: lex.mll parse.mli ast.ml
	ocamllex lex.mll

parse.mli : parse.mly ast.mli 
	menhir --dump --explain --infer parse.mly
#	menhir --dump --explain --strict --infer tpParse.mly

clean:
	rm -rf  tp testLex compilateur testsVerifsContext *.o *.cmi *.cmo *.cmx *~ $(GENERATED) out.txt
