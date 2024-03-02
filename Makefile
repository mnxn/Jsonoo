build:
	dune build
.PHONY: build

watch:
	dune build -w @default @doc @runtest
.PHONY: watch

clean:
	dune clean
	rm -rf ./docs
.PHONY: clean

test:
	dune test
.PHONY: test
