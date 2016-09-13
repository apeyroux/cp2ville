CREATE DATABASE villes;

\c villes

CREATE TABLE villes(
   ID           SERIAL PRIMARY KEY,
   INSE         TEXT  NOT NULL,
   NAME         TEXT  NOT NULL,
   LIGNE5       TEXT  NOT NULL,
   ACHEMINEMENT TEXT  NOT NULL,
   CP           TEXT  NOT NULL
);
