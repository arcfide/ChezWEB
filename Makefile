VERSION=2.0
PREFIX=/usr/local
MACHINE=ta6le
TEXDIR=${PREFIX}/share/texmf
DOCDIR=${PREFIX}/share/doc/chezweb-${VERSION}
BINDIR=${PREFIX}/bin
LIBDIR=${PREFIX}/lib/chezweb

PACKAGE_FILES=README ChangeLog \
    cheztangle chezweave chezweb.pdf doc/cwebman.pdf chezwebmac.tex \
    runtime.so

build: 
	./bootstrap
	./tangleit
	./weaveit
	
petite: 
	./bootstrap
	./tangleit petite
	./weaveit
	@echo ""
	@echo ""
	@echo "******* Remember to set CHEZWEBHOME=${LIBDIR} *******"
	
install:
	env BINDIR=${BINDIR} \
	    TEXDIR=${TEXDIR} \
	    DOCDIR=${DOCDIR} \
	    LIBDIR=${LIBDIR} \
	  ./installit ${PREFIX}

package: build
	rm -rf chezweb-${VERSION}-${MACHINE}
	mkdir chezweb-${VERSION}-${MACHINE}
	cp ${PACKAGE_FILES} chezweb-${VERSION}-${MACHINE}
	tar cvJf chezweb-${VERSION}-${MACHINE}.tar.xz \
	    chezweb-${VERSION}-${MACHINE}
	rm -r chezweb-${VERSION}-${MACHINE}

clean:
	rm -rf runtime.ss runtime.sls cheztangle.ss chezweave.ss chezweb.ss
	rm -rf runtime.so cheztangle chezweave 
	rm -rf chezweb-{1,2,3,4,5}.eps
	rm -rf chezweb.tex chezweb.pdf doc/cwebman.pdf doc/cwebman.log
	rm -rf chezweb.{idx,log,mpx,scn,toc}
	rm -rf doc/cwebman.toc
