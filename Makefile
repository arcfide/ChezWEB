CSV=/usr/lib/csv7.9.4
PETITE=/usr/bin/petite
MACHINE=ta6le
BIN=/usr/local/bin
VERSION=0.9

.SUFFIXES: .boot .ss

build: cheztangle.boot chezweave.boot

package:
	rm -rf chezweb-${VERSION}
	mkdir -p chezweb-${VERSION}
	cp cheztangle.ss chezweave.ss cheztangle.czw chezwebmac.tex \
	  Makefile README chezweb-${VERSION}
	tar cv chezweb-${VERSION} | xz -zc > chezweb-${VERSION}.tar.xz

install: build
	cp cheztangle.boot chezweave.boot ${CSV}/${MACHINE}/
	ln -sf ${PETITE} ${BIN}/cheztangle
	ln -sf ${PETITE} ${BIN}/chezweave

.ss.boot:
	@echo '(make-boot-file "$@" '"'"'("scheme.boot" "petite.boot") "$<")' scheme -q


