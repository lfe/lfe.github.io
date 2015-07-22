PORT ?= 4000
dev/logs:
	mkdir dev/logs

run: dev/logs
	@#~/.lfe/lfe/bin/lfe -boot start_sasl -config ./dev/inets.config
	@#erl -v -boot start_sasl -config ./dev/inets
	@~/.lfe/lfe/bin/lfescript ./dev/server.lfe $(shell pwd) $(PORT)

.PHONY: run

