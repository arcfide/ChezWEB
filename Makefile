CSV=/usr/lib/csv7.9.4
PETITE=/usr/bin/petite
MACHINE=ta6le
BIN=/usr/local/bin

.SUFFIXES: .boot .ss

build: cheztangle.boot chezweave.boot

install: build
	cp cheztangle.boot chezweave.boot ${CSV}/${MACHINE}/
	ln -sf ${PETITE} ${BIN}/cheztangle
	ln -sf ${PETITE} ${BIN}/chezweave

.ss.boot:
	@echo '(make-boot-file "$@" '"'"'("scheme.boot" "petite.boot") "$<")' scheme -q


