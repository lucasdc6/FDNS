.PHONY: run docker

build:
	cabal build

build-run:
	cabal run -- fdns -p 9053

run:
	cabal exec fdns -- -p 9053

docker:
	docker build -t fdns:dev -f docker/Dockerfile .

docker-run:
	docker run --rm -it --network host fdns:dev
