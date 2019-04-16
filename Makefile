
release: binary tarball

binary:
	raco exe --gui -o trace-draw ++lang racket/base -v main.rkt

tarball: binary
	raco distribute td trace-draw
	cp demo* README.md td/
	rm -f trace-draw
	mv td trace-draw
	tar -zcvf trace-draw.tar.gz trace-draw
	rm -rf trace-draw

clean:
	rm -rf *~
