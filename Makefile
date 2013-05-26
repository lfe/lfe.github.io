JEKYLL = /usr/bin/jekyll
SITE_BUILD = ./build
BOOK_SRC = $(SITE_BUILD)/downloads
BOOK_DST = ./downloads
PYTHONPATH = PYTHONPATH=.
PYGMENTS = $(shell python -c "import pygments;print pygments.__path__[0]")
MARKDOWN = $(shell python -c "import markdown;print markdown.__path__[0]")

$(JEKYLL):
	sudo gem update --system
	sudo gem install jekyll
	sudo gem install jekyll-tagging

$(PYGMENTS):
	sudo pip install pygments

$(MARKDOWN):
	sudo pip install markdown

doc-deps: $(JEKYLL) $(PYGMENTS) $(MARKDOWN)

build-books: doc-deps
	@echo "Generating books ..."
	rm -f downloads/*.html downloads/*.mobi downloads/*.markdown
	$(PYTHONPATH) python bin/generateBooks.py

build-site: build-books
	rm -rf $(SITE_BUILD)
	jekyll build -d $(SITE_BUILD)
	cp $(BOOK_SRC)/*.html $(BOOK_DST)/

publish-books: build-books
	git commit \
	downloads/*.markdown \
	downloads/*.html \
	downloads/*.mobi \
	-m "Updated LFE ebooks."

build-mobi: build-site
	-./bin/kindlegen downloads/quick-start.html -c2
	-./bin/kindlegen downloads/user-guide.html -c2
	-./bin/kindlegen downloads/processes-tutorial.html -c2

.PHONY: build-books build-mobi