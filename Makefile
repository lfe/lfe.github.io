JEKYLL = /usr/bin/jekyll
SITE_BUILD = ./build
BOOK_SRC = $(SITE_BUILD)/downloads
BOOK_DST = ./downloads
PYTHONPATH = PYTHONPATH=.
BOOKS = $(shell $(PYTHONPATH) python -c "from scriptlib import config;print config.get_book_names()")
EPUB_BUILD = $(SITE_BUILD)/epub
WKPDF = /usr/bin/wkpdf
VENV = .venv
ACT = $(VENV)/bin/activate

$(VENV):
	virtualenv $(VENV)

clean:
	rm -rf $(VENV) $(SITE_BUILD)

$(JEKYLL):
	sudo gem update --system
	sudo gem install jekyll
	sudo gem install jekyll-tagging

$(WKPDF):
	sudo gem install wkpdf

python-deps:
	. $(ACT) && pip install pygments
	. $(ACT) && pip install markdown
	. $(ACT) && pip install pil

doc-deps: $(VENV) $(JEKYLL) $(WKPDF) python-deps

md: doc-deps
	@echo
	@echo "Generating markdown for ebooks ..."
	rm -f $(BOOK_DST)/*.markdown
	. $(ACT) && $(PYTHONPATH) python bin/generateMD.py

site: md
	@echo
	@echo "Building site and generating html for ebooks ..."
	rm -rf $(SITE_BUILD)
	rm -f $(BOOK_DST)/*.html
	jekyll build -d $(SITE_BUILD)
	cp $(BOOK_SRC)/*.html $(BOOK_DST)/

pdf: site $(WKPDF)
	@echo
	@echo "Generating pdfs for ebooks ..."
	rm -f $(BOOK_DST)/*.pdf
	for BOOK in $(BOOKS); do \
	$(WKPDF) \
	--source $(BOOK_DST)/$$BOOK.html \
	--output $(BOOK_DST)/$$BOOK.pdf; done

epub: site pdf
	@echo
	@echo "Generating epubs for ebooks ..."
	rm -f $(BOOK_DST)/*.epub
	for BOOK in $(BOOKS); do \
	. $(ACT) && $(PYTHONPATH) python bin/generateEPub.py \
	--html=$(BOOK_DST)/$$BOOK.html \
	--archive-path=$(EPUB_BUILD); done
	cp $(EPUB_BUILD)/*.epub $(BOOK_DST)

mobi: epub mobi-fast

mobi-fast:
	@echo
	@echo "Generating mobis for ebooks ..."
	rm -f $(BOOK_DST)/*.mobi
	for BOOK in $(BOOKS); do \
	./bin/kindlegen $(BOOK_DST)/$$BOOK.epub -c2; done

publish-books: build-mobi
	git commit \
	$(BOOK_DST)/*.markdown \
	$(BOOK_DST)/*.html \
	$(BOOK_DST)/*.pdf \
	$(BOOK_DST)/*.epub \
	$(BOOK_DST)/*.mobi \
	-m "Updated LFE ebooks."

contributors:
	git log --pretty=format:"%an"|sort -u

.PHONY: build-books build-epub build-mobi python-deps