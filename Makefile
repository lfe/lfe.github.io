JEKYLL = /usr/bin/jekyll

$(JEKYLL):
	sudo gem update --system
	sudo gem install jekyll
	sudo gem install jekyll-tagging

pygments:
	sudo pip install pygments

doc-deps: $(JEKYLL) pygments

build-site:
	jekyll build

docs: doc-deps build-site

mobi: docs
