all:
	yarn
	git submodule update --init --recursive
	cd elm-mdc && make
	elm make || echo ''
	elm-app build
