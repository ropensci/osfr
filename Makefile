PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

VIGNETTES := $(patsubst vignettes%.Rmd, doc%.html, $(wildcard vignettes/*.Rmd))
PRECOMP_VIGNETTES = $(patsubst %Rmd.orig, %Rmd, $(wildcard vignettes/*.orig))

all: docs check
docs: README.md $(VIGNETTES) doc

build: docs
	R CMD build .

check: build
	R CMD check --as-cran --no-tests $(PKGNAME)_$(PKGVERS).tar.gz
	rm -rf $(PKGNAME).Rcheck/

test:
	Rscript -e "devtools::test()"

doc: README.md vignettes
	Rscript -e "devtools::document()"

build-pkgdown:
	R -e "pkgdown::build_site()"

README.md: README.Rmd
	Rscript -e "devtools::build_readme()"

doc/%.html: vignettes/%.Rmd
	Rscript -e "devtools::build_vignettes()"

# precomputed vignettes
vignettes/%.Rmd: vignettes/%.Rmd.orig
	Rscript -e "knitr::knit('$<', '$@', quiet = TRUE)"

install_deps:
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'

install: install_deps build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

tag:
	@echo "Tagging last commit as v$(PKGVERS)"
	@git tag -a v$(PKGVERS) -m "v$(PKGVERS)"

clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck
