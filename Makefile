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
	
install:
	env BINDIR=${BINDIR} \
	    TEXDIR=${TEXDIR} \
	    DOCDIR=${DOCDIR} \
	    LIBDIR=${LIBDIR} \
	  ./installit ${PREFIX}

