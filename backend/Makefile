all: build

build:
	mkdir -p bin
	stack install cards-with-comrades --local-bin-path bin

watch:
	stack build --file-watch --fast

ghci:
	stack ghci cards-with-comrades:lib

# stack install yesod-bin
devel:
	stack exec -- yesod devel

migration: build
	stack exec -- migration

touch-settings:
	touch Settings.hs
