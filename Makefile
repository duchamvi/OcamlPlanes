# Makefile
SOURCES = types.ml lecture.ml  conflicts.ml backtrack.ml main.ml
TARGET = prog
OCAMLC = ocamlc str.cma unix.cma -g
DEP = ocamldep
OBJS = $(SOURCES:.ml=.cmo)

all: .depend byte clean

byte: $(TARGET)

$(TARGET): $(OBJS)
	$(OCAMLC) -o $@ $^

%.cmi: %.mli
	$(OCAMLC) $<

%.cmo: %.ml
	$(OCAMLC) -c $<

.PHONY: clean

clean:
	rm -f *.cm[io] *~

.depend: $(SOURCES)
	$(DEP) *.mli *.ml > .depend

include .depend
