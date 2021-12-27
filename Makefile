.PHONY: run docker

build:
	cabal build

run:
	(cd src && runghc Main.hs -p 9053)

docker:
	docker build -t fdns:dev -f docker/Dockerfile .

docker-run:
	docker run --rm -it --network host fdns:dev
