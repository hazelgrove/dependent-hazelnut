HTML_FILE=$(shell pwd)/_build/default/bin/index.html

all: build

build:
	dune build bin/images/line.png
	dune build bin/images/hole.png
	dune build bin/images/cursor-hole.png
	dune build bin/main.bc.js
	dune build bin/index.html

plain-url:
	@echo "file://$(HTML_FILE)"

url:
	@echo 'file://wsl$$/Ubuntu$(HTML_FILE)'

clean:
	dune clean
