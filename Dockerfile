FROM haskell:8.8.3 AS builder
COPY . /app
WORKDIR /app
RUN apt-get update; apt-get install libpq-dev -y
RUN cabal update
RUN cabal v2-build celery-man

FROM debian:buster-slim
WORKDIR /app
COPY --from=builder /app/dist-newstyle/build/x86_64-linux/ghc-8.8.3/celery-man-0.1.0.0/x/celery-man/build/celery-man/celery-man .
RUN apt-get update; apt-get install libpq-dev -y
EXPOSE 8081
ENV PG_HOST=127.0.0.1
ENV PG_PORT=5432
ENV PG_USER=postgres
ENV PG_PASSWORD=pw
CMD ["./celery-man"]