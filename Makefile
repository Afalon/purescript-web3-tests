.PHONY: install build compile-contracts deploy test

all: install
	@echo prereqs that are newer than install: $?

install:
	npm install

build:
	./node_modules/.bin/pulp build

compile-contracts:
	./node_modules/.bin/pulp build --src-path compile -m Compile --to compile.js && node compile.js --log-level info; rm compile.js

deploy: compile-contracts build
	./node_modules/.bin/pulp run

test:
	./node_modules/.bin/pulp test
