FROM haskell:8.10 as builder

WORKDIR /opt/fdns

COPY fdns.cabal /opt/fdns/fdns.cabal
RUN cabal update &&\
    cabal build --only-dependencies
COPY . /opt/fdns
RUN cabal install

FROM debian:sid-slim

COPY --from=builder /root/.cabal/bin/fdns /usr/local/bin/fdns
ENTRYPOINT ["/usr/local/bin/fdns"]
CMD ["-p 9053"]
