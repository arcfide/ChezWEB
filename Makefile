PREFIX=/usr
CSV=${PREFIX}/lib/csv8.2
PETITE=${PREFIX}/bin/petite
MACHINE=ta6le
BIN=/usr/local/bin
VERSION=1.3.1
TEXMF=/usr/share/texmf-local/tex/chezweb

.SUFFIXES: .boot .ss

SOURCES=tangle.ss weave.ss 
AUXFILES=Makefile README chezwebmac.tex
SCRIPTS=petite/cheztangle petite/chezweave
GUIDE=guide/chezweb_guide.tex
BOOTFILES=tangle.boot weave.boot
CBFILES=cheztangle.boot chezweave.boot

build: ${BOOTFILES}
	ln -f tangle.boot cheztangle.boot
	ln -f weave.boot chezweave.boot

package:
	rm -rf chezweb-${VERSION}
	mkdir -p chezweb-${VERSION}/guide
	mkdir -p chezweb-${VERSION}/petite
	cp ${SOURCES} chezweb-${VERSION}
	cp ${SCRIPTS} chezweb-${VERSION}/petite
	cp ${AUXFILES} chezweb-${VERSION}
	cp ${GUIDE} chezweb-${VERSION}/guide
	tar cvf chezweb-${VERSION}.tar chezweb-${VERSION}
	xz -fzk chezweb-${VERSION}.tar
	gzip -f chezweb-${VERSION}.tar

bin-package: build
	rm -rf chezweb-${VERSION}-${MACHINE}
	mkdir -p chezweb-${VERSION}-${MACHINE}/guide
	cp ${CBFILES} chezweb-${VERSION}-${MACHINE}
	cp ${AUXFILES} chezweb-${VERSION}-${MACHINE}
	cp ${GUIDE} chezweb-${VERSION}-${MACHINE}/guide
	tar cvf chezweb-${VERSION}-${MACHINE}.tar chezweb-${VERSION}-${MACHINE}
	xz -zfk chezweb-${VERSION}-${MACHINE}.tar
	gzip -f chezweb-${VERSION}-${MACHINE}.tar

clean:
	rm -rf chezweb-${VERSION} chezweb-${VERSION}.tar.{xz,gz}
	rm -rf chezweb-${VERSION}-${MACHINE} chezweb-${VERSION}-${MACHINE}.tar.{xz,gz}
	rm -rf ${BOOTFILES}
	rm -rf ${CBFILES}

chezweb_guide: 
	xetex -papersize=letter guide/chezweb_guide.tex

install: build
	cp cheztangle.boot chezweave.boot ${CSV}/${MACHINE}/
	ln -sf ${PETITE} ${BIN}/cheztangle
	ln -sf ${PETITE} ${BIN}/chezweave
	mkdir -p ${TEXMF}
	cp chezwebmac.tex ${TEXMF}

.ss.boot:
	@echo '(make-boot-file "$@" '"'"'("scheme.boot" "petite.boot") "$<")' | scheme -q


