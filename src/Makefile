MAIN=FPEAnalysis

OBJS = ast.cmo lexer.cmo parser.cmo pprint.cmo util.cmo augment.cmo eval.cmo deriv.cmo analysis.cmo $(MAIN).cmo

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

$(MAIN): $(OBJS)
	ocamlc -o $(MAIN) $(OBJS)

lexer.ml : lexer.mll
	ocamllex -q $<

lexer.cmo : parser.cmi lexer.ml
	ocamlc -c lexer.ml

parser.ml : parser.mly
	menhir -v --infer $<

parser.mli : parser.mly
	menhir -v --infer $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli parser.automaton parser.conflicts $(MAIN)
