INTERFACES = parse.mli
SOURCES    = ast.ml parse.ml lex.ml main.ml
#GENERATED  = parse.ml parse.mli parse.automaton parse.conflicts
GENERATED  = lex.ml parse.ml parse.mli parse.automaton parse.conflicts

testLex : parse.mli lex.ml testLex.ml ast.mli
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o testLex ast.ml parse.ml lex.ml testLex.ml

ast.mli: ast.ml
	ocamlc -c ast.ml

lex.ml: lex.mll parse.mli ast.ml
	ocamllex lex.mll

parse.mli : parse.mly ast.mli 
	menhir --dump --explain --infer parse.mly
#	menhir --dump --explain --strict --infer tpParse.mly

compileur: parse.mli $(SOURCES)
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o compileur $(SOURCES)

clean:
	rm -rf  tp testLex *.o *.cmi *.cmo *.cmx *~ $(GENERATED) out.txt
