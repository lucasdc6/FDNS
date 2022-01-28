.PHONY: run docker

build:
	cabal build

run:
	cabal run -- fdns -p 9053 -c config/simple.yaml

console:
	cabal repl

docker:
	docker build -t fdns:dev -f docker/Dockerfile .

docker-run:
	docker run --rm -it --network host fdns:dev

