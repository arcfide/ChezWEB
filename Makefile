VERSION=2.0
PREFIX=/usr
TEXDIR=${PREFIX}/share/texmf-local
DOCDIR=${PREFIX}/doc/chezweb-${VERSION}
BINDIR=${PREFIX}/bin
LIBDIR=${PREFIX}/lib/chezweb

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

clean:
	rm -rf runtime.ss runtime.sls cheztangle.ss chezweave.ss chezweb.ss
	rm -rf runtime.so cheztangle chezweave 
	rm -rf chezweb-{1,2,3,4,5}.eps
	rm -rf chezweb.tex chezweb.pdf doc/cwebman.pdf doc/cwebman.log
	rm -rf chezweb.{idx,log,mpx,scn,toc}
	rm -rf doc/cwebman.toc
