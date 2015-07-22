PORT ?= 4000
MASTER_NODE ?= dev@localhost
CLIENT_NODE ?= devclient@localhost
LFE_BIN_DIR ?=~/.lfe/lfe/bin

dev: dev/logs
	@$(LFE_BIN_DIR)/lfe -sname $(MASTER_NODE) -noshell -- ./dev/server.lfe $(shell pwd) $(PORT)

run:
	@make dev

connect:
	@echo
	@echo "You will now be connecting to your dev webserver in an Erlang shell."
	@echo "To switch to LFE, simply run the following:"
	@echo "    lfe_shell:server()."
	@echo
	@erl -sname $(CLIENT_NODE) -remsh $(MASTER_NODE)

.PHONY: run dev connect

dev/logs:
	mkdir dev/logs
