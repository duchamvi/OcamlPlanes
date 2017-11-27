# Makefile
SOURCES = types.ml conflicts.ml
TARGET = prog
OCAMLC = ocamlc -g
DEP = ocamldep
OBJS = $(SOURCES:.ml=.cmo)

all: .depend byte

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
