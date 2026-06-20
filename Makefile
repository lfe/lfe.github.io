# Makefile for the LFE Website

# ANSI color codes
BLUE := \033[1;34m
GREEN := \033[1;32m
YELLOW := \033[1;33m
RED := \033[1;31m
CYAN := \033[1;36m
RESET := \033[0m

# Variables
PROJECT_NAME := LFE Website
PUBLISH_DIR := site
PORT := 5093
BINARY := lfesite
GIT_COMMIT := $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")

# Default target
.DEFAULT_GOAL := help

# Help target
.PHONY: help
help:
	@echo ""
	@echo "$(CYAN)╔══════════════════════════════════════════════════════════╗$(RESET)"
	@echo "$(CYAN)║$(RESET) $(BLUE)$(PROJECT_NAME) Build System$(RESET)                                 $(CYAN)║$(RESET)"
	@echo "$(CYAN)╚══════════════════════════════════════════════════════════╝$(RESET)"
	@echo ""
	@echo "$(GREEN)Site:$(RESET)"
	@echo "  $(YELLOW)make build$(RESET)            - Build the site (prerender + sass + tailwind + cobalt)"
	@echo "  $(YELLOW)make serve$(RESET)            - Start dev server with file watching (port $(PORT))"
	@echo "  $(YELLOW)make clean$(RESET)            - Remove build output"
	@echo ""
	@echo "$(GREEN)Development:$(RESET)"
	@echo "  $(YELLOW)make install$(RESET)           - Install $(BINARY) and cobalt"
	@echo "  $(YELLOW)make prerender$(RESET)         - Render _md fields to _html in data files"
	@echo "  $(YELLOW)make validate$(RESET)          - Check data files, front-matter, and layouts"
	@echo "  $(YELLOW)make version-check$(RESET)     - Audit data.written_for version banners on posts"
	@echo "  $(YELLOW)make version-fix$(RESET)       - Fill data.written_for banners on posts (writes files)"
	@echo "  $(YELLOW)make sync-versions$(RESET)     - Regenerate src/_data/lfe_versions.yml from release history"
	@echo "  $(YELLOW)make update-versions$(RESET)   - Pull new LFE/Erlang tags from GitHub and refresh version data"
	@echo "  $(YELLOW)make update-versions-dry$(RESET) - Preview update-versions without writing"
	@echo "  $(YELLOW)make migrate$(RESET)           - One-shot Zola to Cobalt migration"
	@echo ""
	@echo "$(GREEN)Testing & Quality:$(RESET)"
	@echo "  $(YELLOW)make test$(RESET)             - Run $(BINARY) tests"
	@echo "  $(YELLOW)make lint$(RESET)             - Run clippy, format check, and tag/category check"
	@echo "  $(YELLOW)make check-tags-cats$(RESET)  - Ensure no spaces in post tags/categories"
	@echo "  $(YELLOW)make format$(RESET)           - Format Rust code"
	@echo "  $(YELLOW)make check$(RESET)            - lint + test + validate + version-check"
	@echo ""
	@echo "$(GREEN)Spelling:$(RESET)"
	@echo "  $(YELLOW)make spell-check$(RESET)      - Check for spelling errors in markdown"
	@echo "  $(YELLOW)make spell-suggest$(RESET)    - Show spelling suggestions"
	@echo "  $(YELLOW)make add-word$(RESET)         - Add a word to the dictionary (WORD=foo)"
	@echo ""
	@echo "$(GREEN)Information:$(RESET)"
	@echo "  $(YELLOW)make info$(RESET)             - Show build information"
	@echo "  $(YELLOW)make check-tools$(RESET)      - Verify required tools are installed"
	@echo ""
	@echo "$(CYAN)Current status:$(RESET) Branch: $(GIT_BRANCH) | Commit: $(GIT_COMMIT)"
	@echo ""

#############################################################################
###   INFORMATION   #########################################################
#############################################################################

.PHONY: info
info:
	@echo ""
	@echo "$(CYAN)╔══════════════════════════════════════════════════════════╗$(RESET)"
	@echo "$(CYAN)║$(RESET)  $(BLUE)Build Information$(RESET)                                       $(CYAN)║$(RESET)"
	@echo "$(CYAN)╚══════════════════════════════════════════════════════════╝$(RESET)"
	@echo ""
	@echo "$(GREEN)Project:$(RESET)"
	@echo "  Name:           $(PROJECT_NAME)"
	@echo "  Workspace:      $$(pwd)"
	@echo ""
	@echo "$(GREEN)Git:$(RESET)"
	@echo "  Branch:         $(GIT_BRANCH)"
	@echo "  Commit:         $(GIT_COMMIT)"
	@echo ""
	@echo "$(GREEN)Tools:$(RESET)"
	@echo "  Rust:           $$(rustc --version 2>/dev/null || echo 'not found')"
	@echo "  Cobalt:         $$(cobalt --version 2>/dev/null || echo 'not found')"
	@echo "  $(BINARY):        $$($(BINARY) --version 2>/dev/null || echo 'not found')"
	@echo "  Tailwind:       $$(tailwindcss --version 2>/dev/null || echo 'not found')"
	@echo ""

.PHONY: check-tools
check-tools:
	@echo "$(BLUE)Checking for required tools...$(RESET)"
	@command -v rustc >/dev/null 2>&1 && echo "$(GREEN)  ✓ rustc found$(RESET)" || echo "$(RED)  ✗ rustc not found$(RESET)"
	@command -v cargo >/dev/null 2>&1 && echo "$(GREEN)  ✓ cargo found$(RESET)" || echo "$(RED)  ✗ cargo not found$(RESET)"
	@command -v cobalt >/dev/null 2>&1 && echo "$(GREEN)  ✓ cobalt found$(RESET)" || echo "$(RED)  ✗ cobalt not found (install: cargo install cobalt-bin)$(RESET)"
	@command -v $(BINARY) >/dev/null 2>&1 && echo "$(GREEN)  ✓ lfesite found$(RESET)" || echo "$(RED)  ✗ $(BINARY) not found (install: make install)$(RESET)"
	@(command -v tailwindcss >/dev/null 2>&1 || command -v npx >/dev/null 2>&1) && echo "$(GREEN)  ✓ tailwindcss found$(RESET)" || echo "$(RED)  ✗ tailwindcss not found$(RESET)"
	@command -v aspell >/dev/null 2>&1 && echo "$(GREEN)  ✓ aspell found$(RESET)" || echo "$(YELLOW)  ⊙ aspell not found (optional, for spell checking)$(RESET)"

#############################################################################
###   SITE   ################################################################
#############################################################################

LFESITE := $(shell command -v $(BINARY) 2>/dev/null)

.PHONY: ensure-lfesite
ensure-lfesite:
ifndef LFESITE
	@echo "$(YELLOW)→ Installing $(BINARY)...$(RESET)"
	@cargo install --path tools/$(BINARY)
endif

.PHONY: build
build: ensure-lfesite
	@echo "$(BLUE)Building $(PROJECT_NAME)...$(RESET)"
	@$(BINARY) build
	@echo "$(GREEN)✓ Build complete$(RESET)"

.PHONY: serve
serve: ensure-lfesite
	@echo "$(BLUE)Starting dev server on port $(PORT)...$(RESET)"
	@$(BINARY) serve --port $(PORT)

.PHONY: run
run: serve

.PHONY: clean
clean:
	@echo "$(BLUE)Cleaning build output...$(RESET)"
	@rm -rf $(PUBLISH_DIR) _site target/*/$(BINARY)
	@cargo uninstall $(BINARY) 2>/dev/null || true
	@echo "$(GREEN)✓ Clean complete$(RESET)"

#############################################################################
###   DEVELOPMENT   #########################################################
#############################################################################

.PHONY: install
install:
	@echo "$(BLUE)Installing $(BINARY)...$(RESET)"
	@cargo install --path tools/$(BINARY)
	@echo "$(GREEN)✓ $(BINARY) installed$(RESET)"
	@echo "$(BLUE)Installing cobalt...$(RESET)"
	@cargo install cobalt-bin
	@echo "$(GREEN)✓ cobalt installed$(RESET)"

.PHONY: prerender
prerender:
	@echo "$(BLUE)Pre-rendering data files...$(RESET)"
	@$(BINARY) prerender
	@echo "$(GREEN)✓ Prerender complete$(RESET)"

.PHONY: validate
validate:
	@$(BINARY) validate

.PHONY: version-check
version-check:
	@$(BINARY) version-check

.PHONY: version-fix
version-fix:
	@echo "$(YELLOW)⚠ This will write data.written_for into posts$(RESET)"
	@$(BINARY) version-check --fix

.PHONY: sync-versions
sync-versions:
	@$(BINARY) sync-versions

.PHONY: update-versions
update-versions:
	@$(BINARY) update-versions

.PHONY: update-versions-dry
update-versions-dry:
	@$(BINARY) update-versions --dry-run

.PHONY: migrate
migrate:
	@echo "$(YELLOW)⚠ This will modify content files in place$(RESET)"
	@$(BINARY) migrate

#############################################################################
###   TESTING & QUALITY   ###################################################
#############################################################################

.PHONY: test
test:
	@echo "$(BLUE)Running tests...$(RESET)"
	@cargo test --workspace
	@echo "$(GREEN)✓ All tests passed$(RESET)"

.PHONY: lint
lint: check-tags-cats
	@echo "$(BLUE)Running linter checks...$(RESET)"
	@echo "$(CYAN)  • Running clippy...$(RESET)"
	@cargo clippy --workspace -- -D warnings
	@echo "$(GREEN)  ✓ Clippy passed$(RESET)"
	@echo "$(CYAN)  • Checking code formatting...$(RESET)"
	@cargo fmt --all -- --check
	@echo "$(GREEN)  ✓ Format check passed$(RESET)"

# Enforce the no-spaces rule for post tags/categories (spaces break tag URLs,
# e.g. /blog/tags/lfe%20friday/). Scans every tags:/categories: line, strips
# the YAML brackets and comma separators, and fails if any element still
# contains a space. Use hyphens instead (lfe-friday, not "lfe friday").
.PHONY: check-tags-cats
check-tags-cats:
	@echo "$(BLUE)Checking post tags/categories for spaces...$(RESET)"
	@files=$$(grep -rlE '^(tags|categories):' src/posts/ 2>/dev/null); \
	if [ -z "$$files" ]; then echo "$(GREEN)  ✓ no tag/category metadata found$(RESET)"; exit 0; fi; \
	bad=$$(awk '/^(tags|categories):/{l=$$0; sub(/^[a-z]+: *\[/,"",l); sub(/\].*$$/,"",l); gsub(/ *, */,",",l); gsub(/,/,"",l); if(l ~ / /) print FILENAME":"FNR": "$$0}' $$files); \
	if [ -n "$$bad" ]; then \
		echo "$(RED)  ✗ spaces found in tags/categories (use '-' instead):$(RESET)"; \
		echo "$$bad" | sed 's/^/    /'; \
		exit 1; \
	fi; \
	echo "$(GREEN)  ✓ no spaces in tags/categories$(RESET)"

.PHONY: format
format:
	@echo "$(BLUE)Formatting code...$(RESET)"
	@cargo fmt --all
	@echo "$(GREEN)✓ Code formatted$(RESET)"

.PHONY: check
check: lint test validate check-tags-cats version-check
	@echo ""
	@echo "$(GREEN)✓ All checks passed (lint + test + validate + version-check)$(RESET)"
	@echo ""

#############################################################################
###   SPELLING   ############################################################
#############################################################################

.PHONY: spell-check
spell-check:
	@echo "$(BLUE)Checking spelling...$(RESET)"
	@for FILE in $$(find . -name "*.md" -not -path "./node_modules/*" -not -path "./target/*" -not -path "./workbench/*"); do \
	RESULTS=$$(cat $$FILE | aspell -d en_GB --mode=markdown list | sort -u | sed -e ':a' -e 'N;$$!ba' -e 's/\n/, /g'); \
	if [[ "$$RESULTS" != "" ]] ; then \
	echo "$(YELLOW)  ⊙ $$FILE:$(RESET)"; \
	echo "    $$RESULTS"; \
	fi; \
	done
	@echo "$(GREEN)✓ Spell check complete$(RESET)"

.PHONY: spell-suggest
spell-suggest:
	@echo "$(BLUE)Generating spelling suggestions...$(RESET)"
	@for FILE in $$(find . -name "*.md" -not -path "./node_modules/*" -not -path "./target/*" -not -path "./workbench/*"); do \
	RESULTS=$$(cat $$FILE | aspell -d en_GB --mode=markdown list | sort -u ); \
	if [[ "$$RESULTS" != "" ]] ; then \
	echo "$(YELLOW)  ⊙ $$FILE:$(RESET)"; \
	for WORD in $$RESULTS; do \
	echo $$WORD| aspell -d en_GB pipe | tail -2|head -1 | sed -e 's/^/    /'; \
	done; \
	fi; \
	done

add-word: WORD ?= ""
add-word:
	@echo "*$(WORD)\n#" | aspell -a

add-words: WORDS ?= ""
add-words:
	@echo "$(BLUE)Adding words:$(RESET)"
	@for WORD in $$(echo $(WORDS)| tr "," "\n"| tr "," "\n" | sed -e 's/^[ ]*//' | sed -e 's/[ ]*$$//'); \
	do echo "  $(GREEN)✓$(RESET) $$WORD"; \
	echo "*$$WORD\n#" | aspell -a > /dev/null; \
	done
