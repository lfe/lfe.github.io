JEKYLL = /usr/bin/jekyll

$(JEKYLL):
	sudo gem update --system
	sudo gem install jekyll
	sudo gem install jekyll-tagging

pygments:
	sudo pip install pygments

doc-deps: $(JEKYLL) pygments
	sudo pip install markdown

build-site: build-books
	jekyll build

build-books: doc-deps
	python bin/generateBooks.py

publish-books: build-books
	git commit \
	downloads/*.markdown \
	downloads/*.html \
	downloads/*.mobi \
	-m "Updated LFE ebooks."