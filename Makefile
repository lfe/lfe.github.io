JEKYLL = /usr/bin/jekyll

$(JEKYLL):
	sudo gem update --system
	sudo gem install jekyll
	sudo gem install jekyll-tagging

pygments:
	sudo pip install pygments

doc-deps: $(JEKYLL) pygments
	sudo pip install markdown

build-site:
	jekyll build

build-books: doc-deps
	python bin/generateBooksMarkdown.py

build-mobi: build-books build-site
	#run mobi gen