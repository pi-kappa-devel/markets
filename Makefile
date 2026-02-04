PACKAGE_DIR = .
VERSION := $(shell grep '^Version:' DESCRIPTION | awk '{print $$2}')
PKG_NAME := $(shell grep '^Package:' DESCRIPTION | awk '{print $$2}')
TARBALL := $(PKG_NAME)_$(VERSION).tar.gz

configure: configure.ac
	@echo "Configuring package..."
	rm -f configure
	autoreconf
	Rscript -e "devtools::clean_dll()"
	Rscript -e "devtools::document()"

test_units: configure
	Rscript -e "devtools::test()"

test_examples: configure
	Rscript -e "devtools::run_examples(run_dontrun = TRUE, run_donttest = TRUE)"

test: test_units test_examples

readme: configure
	@echo "Generating README from R Markdown..."
	Rscript -e "rmarkdown::render('README.Rmd', output_format = 'github_document')"

web_docs_fast: readme
	@echo "Building web documentation (fast mode)..."
	Rscript -e "pkgdown::build_site(lazy = TRUE)"

web_docs: readme
	@echo "Building web documentation (full mode)..."
	Rscript -e "pkgdown::build_site(lazy = FALSE)"

release: web_docs
	@echo "Running release checks..."
	Rscript -e "devtools::spell_check()"
	Rscript -e "devtools::check_man()"
	Rscript -e "devtools::release_checks()"
	@echo "Building package for release..."
	R CMD build $(PACKAGE_DIR)
	@echo "Checking built package..."
	R CMD check $(TARBALL) --as-cran

clean:
	@echo "Cleaning up..."
	rm -f src/*.o src/*.so src/*.dll src/symbols.rds
	rm -rf *.Rcheck/
	rm -f *.tar.gz
	rm -f README.knit.md README.utf8.md
	rm -f configure
	rm -rf docs/*
	rm -rf man/*.Rd

help:
	@echo "make clean: Clean up generated files"
	@echo "make configure: Configure the package"
	@echo "make github_readme: Generate GitHub README from R Markdown"
	@echo "make help: Show this help message"
	@echo "make release: Run prepare for release and run release checks"
	@echo "make test: Run unit tests and examples"
	@echo "make test_examples: Run examples"
	@echo "make test_units: Run unit tests"
	@echo "make web_docs: Build web documentation"
	@echo "make web_docs_fast: Build web documentation with existing vignettes"
