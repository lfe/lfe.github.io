JEKYLL = /usr/bin/jekyll
SITE_BUILD = ./build
BOOK_SRC = $(SITE_BUILD)/downloads
BOOK_DST = ./downloads
PYTHONPATH = PYTHONPATH=.
PYGMENTS = $(shell python -c "import pygments;print pygments.__path__[0]")
MARKDOWN = $(shell python -c "import markdown;print markdown.__path__[0]")
BOOKS = $(shell $(PYTHONPATH) python -c "from scriptlib import config;print config.get_book_names()")
EPUB_BUILD = $(SITE_BUILD)/epub
WKHTMLTOPDF = /usr/local/bin/wkhtmltopdf

$(JEKYLL):
	sudo gem update --system
	sudo gem install jekyll
	sudo gem install jekyll-tagging

$(PYGMENTS):
	sudo pip install pygments

$(MARKDOWN):
	sudo pip install markdown

$(WKHTMLTOPDF):
	brew install wkhtmltopdf

doc-deps: $(JEKYLL) $(PYGMENTS) $(MARKDOWN) $(WKHTMLTOPDF)

build-books: doc-deps
	@echo "Generating books ..."
	rm -f $(BOOK_DST)/*.html $(BOOK_DST)/*.mobi $(BOOK_DST)/*.markdown
	$(PYTHONPATH) python bin/generateBooks.py

build-site: build-books
	rm -rf $(SITE_BUILD)
	jekyll build -d $(SITE_BUILD)
	cp $(BOOK_SRC)/*.html $(BOOK_DST)/

build-pdf: $(WKHTMLTOPDF)
	echo $(WKHTMLTOPDF)
	for BOOK in $(BOOKS); do \
	$(WKHTMLTOPDF) $(BOOK_DST)/$$BOOK.html $(BOOK_DST)/$$BOOK.pdf; done

build-epub: build-site build-pdf
	for BOOK in $(BOOKS); do \
	$(PYTHONPATH) python bin/generateEPub.py \
	--html=$(BOOK_DST)/$$BOOK.html \
	--archive-path=$(EPUB_BUILD); done
	cp $(EPUB_BUILD)/*.epub $(BOOK_DST)

build-mobi: build-epub
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

.PHONY: build-books build-epub build-mobi
