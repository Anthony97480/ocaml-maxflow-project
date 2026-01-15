.PHONY: all build format edit demo clean

src?=0
dst?=5
graph?=graph5.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build
	@for exe in _build/default/src/*.exe; do \
	  ln -fs "$$exe" .; \
	done

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./ftest11.exe graphs/${graph} $(src) $(dst) outfile
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
