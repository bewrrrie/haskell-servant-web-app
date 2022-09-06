/*
Table of students submissions. Each row contains
- timestamp representing a time of submission;
- name of a student;
- text sent by a student;
- grade for a text written by a student.
*/
CREATE TABLE submissions (
  id SERIAL NOT NULL PRIMARY KEY,
  datetime TIMESTAMP NOT NULL,
  name TEXT NOT NULL,
  text TEXT NOT NULL,
  score INT NOT NULL
);
