CREATE DATABASE appdb;

\connect appdb

CREATE TABLE submissions (
  id INT NOT NULL PRIMARY KEY,
  name TEXT NOT NULL,
  text TEXT NOT NULL,
  score INT NOT NULL
);
