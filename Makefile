
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: check build install

docs:
	-rm NAMESPACE
	Rscript -e "library('devtools'); library('methods'); document('.'); check_doc('.')"

check:
	Rscript -e "library('devtools'); library('methods'); check('.')"

build:
	Rscript -e "library('devtools'); build('.'); build('.', binary=TRUE)"
	cp ../$(PKGNAME)_$(PKGVERS).zip /d/packages/
	cp ../$(PKGNAME)_$(PKGVERS).tar.gz /d/packages/

install:
	Rscript -e "library('devtools'); install('.')"

test: 
	Rscript -e "library('testthat'); library('devtools'); load_all('.'); test('.')"
