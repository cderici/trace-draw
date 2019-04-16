
binary:
	raco exe --gui -o trace-draw ++lang racket/base --vv main.rkt

tarball:
	tar -zcvf trace-draw.tar.gz trace-draw demo* README.md
