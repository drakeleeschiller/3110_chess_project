MODULES=state pieces moves board ai command main
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

check:
	@bash check.sh
	
finalcheck:
	@bash check.sh final

zip:
	zip chess.zip *.ml* *.sh _tags .merlin .ocamlinit LICENSE Makefile

docs:
	mkdir -p _doc
	ocamldoc -d _doc -html enigma.ml

clean:
	ocamlbuild -clean
	rm -rf _doc chess.zip
