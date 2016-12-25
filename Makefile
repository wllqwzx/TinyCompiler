make: 
	g++ src/domFtr.cpp
	mv src/a.out ./a.out
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core src/compile.native