## Type make by itself to see all available targets

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: commands

commands:
	@grep -E '^##' Makefile | sed -e 's/##//g'

## package: run all commands necessary to check, build and install the package
package: check build install

## docs: build documentation with Roxygen2
docs:
	-rm NAMESPACE
	Rscript -e "library('devtools'); library('methods'); document('.'); check_doc('.')"

## check: run devtools::check() (builds and runs R CMD check)
check:
	Rscript -e "library('devtools'); library('methods'); check('.')"

## build: build the package and put binaries in /d/packages
build:
	Rscript -e "library('devtools'); build('.'); build('.', binary=TRUE)"
	cp ../$(PKGNAME)_$(PKGVERS).zip /d/packages/
	cp ../$(PKGNAME)_$(PKGVERS).tar.gz /d/packages/

## install: install the package
install:
	Rscript -e "library('devtools'); install('.')"

## test: Run all the tests in tests/testthat
test: 
	Rscript -e "library('testthat'); library('devtools'); load_all('.'); test('.')"
