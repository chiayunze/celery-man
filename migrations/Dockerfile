FROM haskell:8.8.3
RUN apt-get update; apt-get install libpq-dev -y
RUN cabal update; cabal install postgresql-simple-migration
WORKDIR /workdir
ENTRYPOINT ["/bin/sh", "-c"]