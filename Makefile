.PHONY: run docker

run:
	(cd src && runghc Main.hs -p 9053)

docker:
	docker build -t fdns:latest -f docker/Dockerfile .
