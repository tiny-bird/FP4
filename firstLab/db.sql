CREATE TABLE source
(
    id SERIAL PRIMARY KEY,
    title VARCHAR(20) NOT NULL
);

CREATE TABLE author
(
    id      SERIAL PRIMARY KEY,
    name    VARCHAR(20) NOT NULL,
    surname VARCHAR(20) NOT NULL,
    sourceId INT REFERENCES source (id)
);

CREATE TABLE text_info
(
    id       SERIAL PRIMARY KEY,
    type     VARCHAR(20) NOT NULL,
    text     VARCHAR(500) NOT NULL,
    sourceId INT REFERENCES source (id)
);


CREATE TABLE source_terms
(
     id        SERIAL PRIMARY KEY,
     terms VARCHAR(100) NOT NULL,
     purpose VARCHAR(20)                            NOT NULL,
     sourceId INT REFERENCES source (id)

);

INSERT INTO source (title)
VALUES ('source1'),
        ('source2'),
        ('source3');

INSERT INTO author (name, surname, sourceID)
VALUES  ('Albert', 'Einstein', 1),
        ('Marie', 'Curie', 1),
        ('Nikola', 'Tesla', 2);


INSERT INTO text_info (type, text, sourceID)
VALUES ('biography', 'text1...', 1),
        ('fiction', 'text2...', 2);

INSERT INTO source_terms (terms, purpose, sourceid)
VALUES ('Rule1', 'some info...', 1),
        ('Rule2', 'some info2...', 2);
