PWD = $(shell pwd)
UID = $(shell id -u)
GID = $(shell id -g)

PUBLISH_DIR = site
BRANCH = main
TMP_GIT_DIR = /tmp/lfe-io-site-git
PORT = 5099

#############################################################################
###   DOCKER   ##############################################################
#############################################################################

ZOLA_VERS=0.16
NODE_VERS=20
ALPINE_VERS=3.17
DOCKER_TAG=zola$(ZOLA_VERS)-node$(NODE_VERS)-alpine$(ALPINE_VERS)
MOUNT_DIR = /app

docker-build:
	docker build -t lfe:$(DOCKER_TAG) .

docker-shell:
	docker run -it \
	--entrypoint ash \
	lfe:$(DOCKER_TAG)

#############################################################################
###   SITE   ################################################################
#############################################################################

build: docker-build tailwind-build clean
	@echo " >> Building site ..."
	docker run \
	-u "$(UID):$(GID)" -v $(PWD):$(MOUNT_DIR) --workdir $(MOUNT_DIR) lfe:$(DOCKER_TAG) \
	build -o $(PUBLISH_DIR)

serve: docker-build
	@echo " >> Running site ..."
	@docker run \
	-p 8080:8080 -p 1024:1024 \
	-u "$(UID):$(GID)" -v $(PWD):$(MOUNT_DIR) --workdir $(MOUNT_DIR) lfe:$(DOCKER_TAG) \
	serve --interface 0.0.0.0 --port 8080 --base-url localhost

run: serve

clean:
	@echo " >> Removing files from site dir ..."
	@rm -rf $(PUBLISH_DIR)

$(PUBLISH_DIR)/CNAME:
	@echo " >> Copying CNAME File ..."
	@cp CNAME $(PUBLISH_DIR)/

publish: build $(PUBLISH_DIR)/CNAME
	@echo " >> Publishing site ..."
	@git commit -am "Updated content"
	@git push origin $(BRANCH)

#############################################################################
###   SPELLING   ############################################################
#############################################################################

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

#############################################################################
###   TAILWIND   ############################################################
#############################################################################

TAILWIND_BASE = styles
TAILWIND_INPUT = $(TAILWIND_BASE)/site.css
TAILWIND_OUTPUT = static/css/site.css

tailwind-setup: tailwind.config.js $(TAILWIND_BASE)/components

$(TAILWIND_BASE)/components:
	mkdir -p $(TAILWIND_BASE) && \
	cd $(TAILWIND_BASE) && \
	git clone git@github.com:merakiui/merakiui.git && \
	mv merakiui/components merakiui/assets . && \
	rm -rf merakiui

tailwind.config.js:
	docker run -it \
	-v $(PWD):$(MOUNT_DIR) --workdir $(MOUNT_DIR) \
	--entrypoint npx \
	lfe:$(DOCKER_TAG) \
	tailwindcss init

tailwind-build:
	docker run -it \
	-v $(PWD):$(MOUNT_DIR) --workdir $(MOUNT_DIR) \
	--entrypoint npx \
	lfe:$(DOCKER_TAG) \
	tailwindcss -i $(TAILWIND_INPUT) -o $(TAILWIND_OUTPUT)
