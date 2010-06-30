PREFIX=/usr
CSV=${PREFIX}/lib/csv8.0
PETITE=${PREFIX}/bin/petite
MACHINE=ta6le
BIN=/usr/local/bin
VERSION=1.1-beta
TEXMF=/usr/share/texmf-local/tex/chezweb

.SUFFIXES: .boot .ss

build: cheztangle.boot chezweave.boot

package:
	rm -rf chezweb-${VERSION}
	mkdir -p chezweb-${VERSION}
	cp cheztangle.ss chezweave.ss chezwebmac.tex \
	  Makefile README chezweb-${VERSION}
	tar cvf chezweb-${VERSION}.tar chezweb-${VERSION}
	xz -fzk chezweb-${VERSION}.tar
	gzip -f chezweb-${VERSION}.tar

bin-package: cheztangle.boot chezweave.boot
	rm -rf chezweb-${VERSION}-${MACHINE}
	mkdir -p chezweb-${VERSION}-${MACHINE}
	cp cheztangle.boot chezweave.boot chezwebmac.tex README Makefile \
          chezweb-${VERSION}-${MACHINE}
	tar cvf chezweb-${VERSION}-${MACHINE}.tar chezweb-${VERSION}-${MACHINE}
	xz -zfk chezweb-${VERSION}-${MACHINE}.tar
	gzip -f chezweb-${VERSION}-${MACHINE}.tar

clean:
	rm -rf chezweb-${VERSION} chezweb-${VERSION}.tar.{xz,gz}
	rm -rf chezweb-${VERSION}-${MACHINE} chezweb-${VERSION}-${MACHINE}.tar.{xz,gz}
	rm -rf cheztangle.boot chezweave.boot

install: build
	cp cheztangle.boot chezweave.boot ${CSV}/${MACHINE}/
	ln -sf ${PETITE} ${BIN}/cheztangle
	ln -sf ${PETITE} ${BIN}/chezweave
	mkdir -p ${TEXMF}
	cp chezwebmac.tex ${TEXMF}

.ss.boot:
	@echo '(make-boot-file "$@" '"'"'("scheme.boot" "petite.boot") "$<")' | scheme -q


