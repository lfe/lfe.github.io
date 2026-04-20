PUBLISH_DIR = site
PORT = 3000

#############################################################################
###   SITE   ################################################################
#############################################################################

build:
	@lfesite build

serve:
	@lfesite serve --port $(PORT)

run: serve

clean:
	@echo " >> Removing build output ..."
	@rm -rf $(PUBLISH_DIR) _site

#############################################################################
###   DEVELOPMENT   #########################################################
#############################################################################

install:
	@echo " >> Installing lfesite ..."
	@cargo install --path tools/lfesite
	@echo " >> Installing cobalt ..."
	@cargo install cobalt-bin

prerender:
	@lfesite prerender

migrate:
	@lfesite migrate

#############################################################################
###   SPELLING   ############################################################
#############################################################################

spell-check:
	@for FILE in `find . -name "*.md" -not -path "./node_modules/*" -not -path "./target/*"`; do \
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
	@for FILE in `find . -name "*.md" -not -path "./node_modules/*" -not -path "./target/*"`; do \
	RESULTS=$$(cat $$FILE | aspell -d en_GB --mode=markdown list | sort -u ); \
	if [[ "$$RESULTS" != "" ]] ; then \
	echo "Potential spelling errors in $$FILE:"; \
	for WORD in $$RESULTS; do \
	echo $$WORD| aspell -d en_GB pipe | tail -2|head -1 | sed -e 's/^/    /'; \
	done; \
	echo; \
	fi; \
	done
