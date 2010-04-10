CSV=/usr/lib/csv8.0
PETITE=/usr/bin/petite
MACHINE=ta6le
BIN=/usr/local/bin
VERSION=1.0
TEXMF=/usr/share/texmf-local/tex/chezweb

.SUFFIXES: .boot .ss

build: cheztangle.boot chezweave.boot

package:
	rm -rf chezweb-${VERSION}
	mkdir -p chezweb-${VERSION}
	cp cheztangle.ss chezweave.ss cheztangle.czw chezwebmac.tex \
	  Makefile README chezweb-${VERSION}
	tar cv chezweb-${VERSION} | xz -zc > chezweb-${VERSION}.tar.xz

clean:
	rm -rf chezweb-${VERSION} chezweb-${VERSION}.tar.xz
	rm -rf cheztangle.boot chezweave.boot

install: build
	cp cheztangle.boot chezweave.boot ${CSV}/${MACHINE}/
	ln -sf ${PETITE} ${BIN}/cheztangle
	ln -sf ${PETITE} ${BIN}/chezweave
	mkdir -p ${TEXMF}
	cp chezwebmac.tex ${TEXMF}

.ss.boot:
	@echo '(make-boot-file "$@" '"'"'("scheme.boot" "petite.boot") "$<")' | scheme -q


