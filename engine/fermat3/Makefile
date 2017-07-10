# Makefile for GNU FermaT v1.0

ARCH := $(shell uname)
CC = gcc
OPT = -O2

all: $(ARCH)/scmfmt

install: $(ARCH)/scmfmt
	@echo "See the files DOIT.sh, DOUT.bash or DOIT.csh"
	@echo "for path and environment settings"


$(ARCH)/scmfmt:
	rm -f scm/slibcat
	cd src; perl -I"`pwd`/../config" ../bin/make-fermat -FermaT "`pwd`/.." -install



clean:
	rm -f src/ALL.* src/gambit.scm src/defs1.scm \
		src/*/*.scx src/*/*.[cho] \
		src/scm.c src/scm.o \
		scm/slibcat
	rm -rf $(ARCH)


test: $(ARCH)/scmfmt
	@echo Testing basic WSL operation:
	perl -I"`pwd`/config" ./bin/test-fermat -FermaT "`pwd`"
	@echo Testing transformations:
	perl -I"`pwd`/config" ./bin/test-trans -FermaT "`pwd`"


.PHONY: test
