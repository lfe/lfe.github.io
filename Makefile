JEKYLL = /usr/bin/jekyll
BOOK_SRC = ./build/downloads
BOOK_DST = ./downloads

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
	python bin/generateBooks.py
	cp $(BOOK_SRC)/*.html $(BOOK_DST)/

build-site: build-books
	jekyll build

publish-books: build-books
	git commit \
	downloads/*.markdown \
	downloads/*.html \
	downloads/*.mobi \
	-m "Updated LFE ebooks."