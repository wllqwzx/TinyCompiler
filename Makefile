make: a.out
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core src/compile.native

a.out:
	g++ ./src/domFtr.cpp -o a.out
	mv ./src/a.out ./a.out
