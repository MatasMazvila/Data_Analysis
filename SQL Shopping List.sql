CREATE TABLE groceries (
ID INTEGER PRIMARY KEY,
name TEXT,
quantity INTEGER);

INSERT INTO groceries VALUES (1, 'Bananas', 4);
INSERT INTO groceries VALUES (2, 'Peanut Butter', 1);
INSERT INTO groceries VALUES (3, 'Dark Chocolate bars', 2);

SELECT * FROM groceries;