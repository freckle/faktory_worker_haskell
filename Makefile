all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build --fast --pedantic --test

.PHONY: lint
lint:
	stack exec hlint library tests
	stack exec weeder .

.PHONY: clean
clean:
	stack clean
