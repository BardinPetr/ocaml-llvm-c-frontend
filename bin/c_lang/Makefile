all:
	ocamllex c_lexer.mll
	ocamlyacc c_parser.mly
	ocamlc -c c_syntax.ml
	ocamlc -c c_parser.mli
	ocamlc -c c_lexer.ml
	ocamlc -c c_parser.ml
	ocamlc -c main.ml
	ocamlmktop c_syntax.cmo c_lexer.cmo c_parser.cmo main.cmo -o test.top


clean:
	rm c_lexer.ml c_parser.ml *.cmo *.mli *.cmi