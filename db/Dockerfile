FROM postgres:latest

ENV POSTGRES_USER "user"
ENV POSTGRES_PASSWORD "pass"

COPY docker-entrypoint-initdb.d/ ./docker-entrypoint-initdb.d/
