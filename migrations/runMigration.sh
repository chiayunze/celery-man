#!/bin/bash
# make sure that PG_USER, PG_PASSWORD, PG_HOST, PG_PORT are in your environment variables

migrate init postgres://${PG_USER}:${PG_PASSWORD}@${PG_HOST}:${PG_PORT}
migrate migrate postgres://${PG_USER}:${PG_PASSWORD}@${PG_HOST}:${PG_PORT} scripts