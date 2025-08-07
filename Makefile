PACKAGE_DIR = .

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

github_readme: configure
	@echo "Generating GitHub README from R Markdown..."
	Rscript -e "rmarkdown::render('README.Rmd', output_format = 'github_document')"

_web_docs_pre: configure
	@echo "Preparing web documentation..."
	Rscript -e "devtools::document()"
	Rscript -e "rmarkdown::render('README.Rmd', output_format = 'md_document')"

_web_docs_post: github_readme
	@echo "Post-processing web documentation..."

web_docs_fast: _web_docs_pre
	@echo "Building web documentation (fast mode)..."
	Rscript -e "pkgdown::build_site(lazy = TRUE)"
	$(MAKE) _web_docs_post

web_docs: _web_docs_pre
	@echo "Building web documentation (full mode)..."
	Rscript -e "pkgdown::build_site(lazy = FALSE)"
	$(MAKE) _web_docs_post

release: web_docs
	@echo "Running release checks..."
	Rscript -e "devtools::spell_check()"
	Rscript -e "devtools::check_man()"
	Rscript -e "devtools::release_checks()"

help:
	@echo "make configure: Configure the package"
	@echo "make test_units: Run unit tests"
	@echo "make test_examples: Run examples"
	@echo "make test: Run unit tests and examples"
	@echo "make github_readme: Generate GitHub README from R Markdown"
	@echo "make web_docs_fast: Build web documentation with existing vignettes"
	@echo "make web_docs: Build web documentation"
	@echo "make release: Run prepare for release and run release checks"
	@echo "make help: Show this help message"
