.PHONY: install build compile-contracts deploy test

# see https://stackoverflow.com/a/26936855/1798418
PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash


help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: install
	@echo prereqs that are newer than install: $?

install: ## Install all dependencies
	bower install & npm install & wait

build: ## Builds the application
	pulp build

compile-contracts: ## Compile contracts
	chanterelle build

deploy: ## Deploy contracts
	chanterelle deploy ./output/Main/index.js

test: ## Test contracts
	pulp test
