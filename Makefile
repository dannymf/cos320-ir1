COMPILE = bin/sdt.exe

.PHONY: all clean zip

all:
	dune build $(COMPILE)
	cp $(COMPILE) compile.exe

sum.s: compile.exe
	./compile.exe sum > $@

product.s: compile.exe
	./compile.exe product > $@

square.s: compile.exe
	./compile.exe square > $@

perimeter.s: comile.exe
	./compile.exe perimeter > $@


sum_instrs.s: compile.exe
	./compile.exe sum_instrs > $@

early_exit.s: compile.exe
	./compile.exe early_exit > $@

factorial.s: compile.exe
	./compile.exe factorial > $@


%.exe: %.s
	gcc $< bin/runtime.c -o $@

clean:
	dune clean
	rm *.exe *.s *.zip
	find . -name "*~" -delete

zip:
	zip IR1.zip bin/* lib/x86/* dune-project ir.opam Makefile
