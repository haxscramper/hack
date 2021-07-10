CREATE TABLE entries (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    parentId INTEGER references entries(id),
    depth INTEGER NOT NULL,
    name TEXT NOT NULL,
    kind INTEGER,
    docRaw TEXT,
    docParsed BLOB,
    -- Optional type uses in different entries
    typeUse INTEGER REFERENCES typeInstances(id),
    declHead INTEGER REFERENCES locations(id),
    -- procType INTEGER REFERENCES procedureSignatures(id)
);

CREATE TABLE identParts (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    entry INTEGER NOT NULL REFERENCES entries(id),
    partKind INTEGER NOT NULL
);

CREATE TABLE files (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    content TEXT
);

CREATE TABLE uses(
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    kind INTEGER NOT NULL,
    location INTEGER REFERENCES locations(id)
);

CREATE TABLE locations (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    fileId INTEGER REFERENCES files(id),
    line INTEGER NOT NULL,
    startCol INTEGER NOT NULL,
    endCol INTEGER NOT NULL,
    endLine INTEGER
);

CREATE TABLE typeInstances (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    -- Can be NULL when generating from `--fastdoc` or for DocType rows like
    -- 'value'
    usedType INTEGER REFERENCES entries(id),
    -- Different variants of indirect type uses
    kind INTEGER,
    -- Unwrapping layers for indirection for pointers, references and other
    --  aux helpers
    useKind INTEGER REFERENCES indirectUses(id)
);

CREATE TABLE genericSpecializations (
    baseType INTEGER NOT NULL REFERENCES entries(id),
    paramPos INTEGER NOT NULL,
    type INTEGER NOT NULL REFERENCES typeInstances(id)
);

CREATE TABLE argLists (
    id INTEGER NOT NULL,                             -- Arg list id
    pos INTEGER NOT NULL,                         -- Argument positin
    name TEXT NOT NULL,                              -- Argument name
    type INTEGER NOT NULL REFERENCES typeInstances(id) -- Arg type
);

CREATE TABLE pragmaLists (
    id INTEGER UNIQUE PRIMARY KEY NOT NULL,
    pragma INTEGER NOT NULL REFERENCES entries(id)
);

CREATE TABLE procedureSignatures (
    id INTEGER UNIQUE PRIMARY KEY NOT NULL,
    returnType INTEGER REFERENCES typeInstances(id),
    argList INTEGER REFERENCES argLists(id),
    pragmaList INTEGER REFERENCES pragmaLists(id)
);

CREATE TABLE indirectUses (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    serialized TEXT
);

CREATE TABLE nested (
    parent INTEGER REFERENCES entries(id),
    sub INTEGER REFERENCES entries(id)
);

-- code.txt
-- func other(a: int, b: float): string = ## documentation
-- func other(c: string) =
-- func other(c: int) =


INSERT INTO entries(id, name, depth) VALUES
    (20, "int",    0),
    (21, "float",  0),
    (22, "string", 0)
    ;


INSERT INTO typeInstances(id, usedType) VALUES
    (0, 20),
    (1, 21),
    (2, 22)
    ;


INSERT INTO argLists(id, pos, name, type) VALUES
    (1, 0, "a", 20),
    (1, 1, "b", 21),
    (2, 0, "c", 22),
    (3, 0, "c", 20)
    ;

INSERT INTO procedureSignatures(id, returnType, argList, pragmaList) VALUES
    (1, 2,    1, NULL),
    (2, NULL, 2, NULL),
    (3, NULL, 3, NULL)
    ;

INSERT INTO entries
    (id, name,  kind, docRaw,          procType, typeUse, parentId, depth) VALUES
    (1,  "code.txt", 0,    NULL,            NULL,     NULL,    NULL,     0),
    (2,  "other",    1,    "documentation", 1,        NULL,    1,        1),
    (3,  "a",        2,    NULL,            NULL,     0,       2,        2),
    (4,  "b",        2,    NULL,            NULL,     1,       2,        2),
    (5,  "other",    1,    NULL,            2,        NULL,    1,        1),
    (6,  "c",        2,    NULL,            NULL,     22,      5,        2),
    (7,  "other",    1,    NULL,            3,        NULL,    1,        1),
    (8,  "c",        2,    NULL,            NULL,     21,      7,        2)
    ;

INSERT INTO nested(parent, sub) VALUES
    (1, 2), -- functions is located in code.txt
    (2, 3), -- has two arguments "a" (id 3)
    (2, 4), -- and "b" (id 4)
    (1, 5),
    (5, 6),
    (1, 7),
    (7, 8)
    ;

.headers on
.mode column
.nullvalue [NULL]



-- Get all entries with first argument matching particular type
select * from entries as entry
where entry.procType in (
  select
    proc.id

  from
    argLists as arg
    join procedureSignatures as proc
    on arg.id = proc.argList

  where arg.type in (20, 21)
);


-- All procedure entries with name
select * from entries
where kind = 1 and name = "other";


-- Select all procedures
WITH RECURSIVE cte (id, depth, parentId, name) as (
  SELECT entry.id, entry.depth, entry.parentId, entry.name
  FROM entries entry
  WHERE entry.name = "code.txt"

  UNION ALL

  SELECT entry.id, entry.depth, entry.parentId, entry.name
  FROM entries AS entry JOIN cte
  ON cte.id = entry.parentId
  WHERE
    CASE entry.depth
      WHEN 0 THEN entry.name = "code.txt"
      WHEN 1 THEN
        entry.name = "other" AND
        entry.procType IN (
          SELECT proc.id

          FROM argLists AS arg
          JOIN
            procedureSignatures AS proc

          ON arg.id = proc.argList

          WHERE
            CASE arg.pos
              WHEN 0 THEN arg.type = 20
              WHEN 1 THEN arg.type = 21
              ELSE 0
            END
          GROUP BY arg.id
          HAVING COUNT(DISTINCT pos) = 2
        )
      ELSE 0
    END

)

SELECT * FROM cte;
