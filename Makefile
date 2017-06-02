.PHONY: help bench test bench/%.exs

APP_NAME ?= libgraph
VERSION ?= `cat mix.exs | grep "version:" | cut -d '"' -f2`
BENCHMARKS := $(wildcard bench/*.exs)

help:
	@echo "$(APP_NAME):$(VERSION)"
	@perl -nle'print $& if m{^[a-zA-Z_-]+:.*?## .*$$}' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

test: ## Run tests
	mix test

bench/%.exs:
	mix run $@

$(BENCHMARKS): bench/%.exs

bench: $(BENCHMARKS) ## Run benchmarks
	@echo "Benchmarks complete!"
