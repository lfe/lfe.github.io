BIN = zola
GEN := $(shell which $(BIN) 2> /dev/null)
PUBLISH_DIR = site
PUBLISH_BRANCH = main
BUILDER_BRANCH = builder
TMP_GIT_DIR = /tmp/lfe-io-site-git
PORT = 5099

define BINARY_ERROR

No $(BIN) found in Path.

Install $(BIN):

	$ brew install $(BIN)

endef

build:
ifndef GEN
	$(error $(BINARY_ERROR))
endif
	@echo " >> Building site ..."
	@$(MAKE) backup-submodule-git
	@$(GEN) build -o $(PUBLISH_DIR)
	@$(MAKE) restore-submodule-git

serve:
	@bash -c "trap \"$(MAKE) serve-cleanup\" EXIT; $(GEN) serve -o $(PUBLISH_DIR) -p $(PORT)"

serve-cleanup: site-init build

run: serve

clean:
	@echo " >> Removing files from site dir ..."
	@rm -rf $(PUBLISH_DIR)/*

site-init:
	@git submodule update --init --recursive
	@cd $(PUBLISH_DIR) && git checkout $(PUBLISH_BRANCH)

$(PUBLISH_DIR)/README.md:
	@echo " >> Creating static site's README ..."
	@echo '# Content for the LFE site' > $(PUBLISH_DIR)/README.md
	@echo 'Published at [lfe.io/](https://lfe.io/)' >> $(PUBLISH_DIR)/README.md
	@cd $(PUBLISH_DIR) && git add README.md

$(PUBLISH_DIR)/CNAME:
	@echo " >> Copying CNAME File ..."
	@cp CNAME $(PUBLISH_DIR)/

publish: clean build $(PUBLISH_DIR)/README.md $(PUBLISH_DIR)/CNAME
	@echo " >> Publishing site ..."
	@git commit -am "Updated content" && \
	git push origin $(BUILDER_BRANCH)

spell-check:
	@for FILE in `find . -name "*.md"`; do \
	RESULTS=$$(cat $$FILE | aspell -d en_GB --mode=markdown list | sort -u | sed -e ':a' -e 'N;$$!ba' -e 's/\n/, /g'); \
	if [[ "$$RESULTS" != "" ]] ; then \
	echo "Potential spelling errors in $$FILE:"; \
	echo "$$RESULTS" | \
	sed -e 's/^/    /'; \
	echo; \
	fi; \
	done

add-word: WORD ?= ""
add-word:
	@echo "*$(WORD)\n#" | aspell -a

add-words: WORDS ?= ""
add-words:
	@echo "Adding words:"
	@for WORD in `echo $(WORDS)| tr "," "\n"| tr "," "\n" | sed -e 's/^[ ]*//' | sed -e 's/[ ]*$$//'`; \
	do echo "  $$WORD ..."; \
	echo "*$$WORD\n#" | aspell -a > /dev/null; \
	done
	@echo

spell-suggest:
	@for FILE in `find . -name "*.md"`; do \
	RESULTS=$$(cat $$FILE | aspell -d en_GB --mode=markdown list | sort -u ); \
	if [[ "$$RESULTS" != "" ]] ; then \
	echo "Potential spelling errors in $$FILE:"; \
	for WORD in $$RESULTS; do \
	echo $$WORD| aspell -d en_GB pipe | tail -2|head -1 | sed -e 's/^/    /'; \
	done; \
	echo; \
	fi; \
	done
