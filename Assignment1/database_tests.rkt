#| Assignment 1 - Racket Query Language Tests (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student id for each of your group members below.***

Joseph Cameron, camer249, 1003699081

|#

; Note the use of plai here; this is required for "test"
#lang plai
(abridged-test-output #t)

; This imports your file; do not change this line!
(require "database.rkt")

; Test helpers - use these instead of the built-in syntactic forms.
; DON'T export them from database.rkt!
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

;
; Correction Oct 5 2016

;(define-syntax If
;  (syntax-rules ()
;  ((If a b c)
;  (if a b c))))

;(define-syntax And
;  (syntax-rules ()
;    [(And p q)
;     (if (not p)
;         #f
;         q)]))

;(define-syntax Or
;  (syntax-rules ()
;    [(Or p q)
;     (if p
;         #t
;         q)]))


; Please do define And, Or as syntactic forms
; We have actually done this in class you may use the class code and this week's lab code for this.

; Sample tables - add more tables!
; Ideas: empty table; table with just 1 attribute; 0 attributes; duplicates
(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))

;-------------------------
;New Tables defined by me:

;Empty Table
(define Empty 
  '(()
    ()))

;Table with no tuples
(define AttributesOnly
  '(("Column1" "Column2")))

; Table with 1 attribute
(define OneAttribute
  '(("Column")
    ("Tuple1")
    ("Tuple2")))

; Table with one tuple
(define OneTuple
  '(("Name" "Age" "LikesChocolate")
    ("David" 20 #t)))

; Table with duplicates
(define Duplicate
  '(("Name" "Age" "LikesChocolate")
    ("David" 20 #t)
    ("David" 20 #t)
    ("Jen" 30 #f)
    ("Jen" 30 #f)))

;-------------------------

#|
All tests go below. 
We have divided our tests into five sections:
- No WHERE/ORDER BY
- WHERE clause, but no ORDER BY
- ORDER BY clause, but no WHERE
- Both WHERE and ORDER BY
- Nested queries

Please respect this categorization when you add your tests,
and your TAs will appreciate it!
|#


; ---- SELECT/FROM tests ----
; Select all
(test (SELECT * FROM Person)
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Reordering columns
(test (SELECT '("Age" "LikesChocolate" "Name") FROM Person)
      '(("Age" "LikesChocolate" "Name")
        (20 #t "David") 
        (30 #t "Jen") 
        (100 #f "Paul")))

; Select creates duplicates
(test (SELECT '("Name") FROM Teaching)
      '(("Name")
        ("David")
        ("Paul")
        ("David")))

; Select given a literal table
(test
 (SELECT '("A" "B")
   FROM '(("C" "A" "B" "D")
          (1 "Hi" 5 #t)
          (2 "Bye" 5 #f)
          (3 "Hi" 10 #t)))
 '(("A" "B")
   ("Hi" 5)
   ("Bye" 5)
   ("Hi" 10)))

; Select all from two product of two tables
(test (SELECT * FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #T "David" "CSC343")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")))

; Select some from two tables
(test (SELECT '("P.Name" "Course" "Age") FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Course" "Age")
        ("David" "CSC324" 20)
        ("David" "CSC108" 20)
        ("David" "CSC343" 20)
        ("Jen" "CSC324" 30)
        ("Jen" "CSC108" 30)
        ("Jen" "CSC343" 30)
        ("Paul" "CSC324" 100)
        ("Paul" "CSC108" 100)
        ("Paul" "CSC343" 100)))

; Take the product of a table with itself
(test (SELECT '("E.Course" "E1.Course") FROM [Teaching "E1"] [Teaching "E"])
      '(("E.Course" "E1.Course")
        ("CSC324" "CSC324")
        ("CSC108" "CSC324")
        ("CSC343" "CSC324")
        ("CSC324" "CSC108")
        ("CSC108" "CSC108")
        ("CSC343" "CSC108")
        ("CSC324" "CSC343")
        ("CSC108" "CSC343")
        ("CSC343" "CSC343")))

; Take the product of a literal table with an identifier
(test
 (SELECT *
   FROM ['(("Age" "A" "Name" "D")
           (1 "Hi" 5 #t)
           (2 "Bye" 5 #f)
           (3 "Hi" 10 #t))
         "T1"]
        [Person "T2"])
 '(("T1.Age" "A" "T1.Name" "D" "T2.Name" "T2.Age" "LikesChocolate")
   (1 "Hi" 5 #t "David" 20 #t)
   (1 "Hi" 5 #t "Jen" 30 #t)
   (1 "Hi" 5 #t "Paul" 100 #f)
   (2 "Bye" 5 #f "David" 20 #t)
   (2 "Bye" 5 #f "Jen" 30 #t)
   (2 "Bye" 5 #f "Paul" 100 #f)
   (3 "Hi" 10 #t "David" 20 #t)
   (3 "Hi" 10 #t "Jen" 30 #t)
   (3 "Hi" 10 #t "Paul" 100 #f)))

;-------------------------
;New Tests defined by me.

; Select all from the empty table.
(test (SELECT * FROM Empty)
      '(()
        ()))

; Select all from the table with no tuples.
(test (SELECT * FROM AttributesOnly)
      '(("Column1" "Column2")))

; Select one attribute from the table with no tuples.
(test (SELECT '("Column1") FROM AttributesOnly)
      '(("Column1")))

;Check that the join with an empty table produces the same result.
(test (SELECT * FROM [Person "P"] [Empty "E"])
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Select all from the table with duplicates.
(test (SELECT * FROM Duplicate)
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("David" 20 #t)
        ("Jen" 30 #f)
        ("Jen" 30 #f)))

; Select all from the table with one attribute.
(test (SELECT * FROM OneAttribute)
      '(("Column")
        ("Tuple1")
        ("Tuple2")))

; Select all from the table with one tuple.
(test (SELECT * FROM OneTuple)
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)))

; Select all from three product of three tables
(test (SELECT * FROM [Person "P"] [Teaching "T"] [OneTuple "O"])
      '(("P.Name" "P.Age" "P.LikesChocolate" "T.Name" "Course" "O.Name" "O.Age" "O.LikesChocolate")
        ("David" 20 #t "David" "CSC324" "David" 20 #t)
        ("David" 20 #t "Paul" "CSC108" "David" 20 #t)
        ("David" 20 #t "David" "CSC343" "David" 20 #t)
        ("Jen" 30 #t "David" "CSC324" "David" 20 #t)
        ("Jen" 30 #t "Paul" "CSC108" "David" 20 #t)
        ("Jen" 30 #T "David" "CSC343" "David" 20 #t)
        ("Paul" 100 #f "David" "CSC324" "David" 20 #t)
        ("Paul" 100 #f "Paul" "CSC108" "David" 20 #t)
        ("Paul" 100 #f "David" "CSC343" "David" 20 #t)))

; Select an attribute from the product of three tables
(test (SELECT '("O.Name") FROM [Person "P"] [Teaching "T"] [OneTuple "O"])
      '(("O.Name")
        ("David")
        ("David")
        ("David")
        ("David")
        ("David")
        ("David")
        ("David")
        ("David")
        ("David")))

;-------------------------

; ---- WHERE ----
; Attribute as condition, select all
(test (SELECT *
        FROM Person
        WHERE "LikesChocolate")
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Jen" 30 #t)))

; Attribute as condition, select subset
(test (SELECT '("LikesChocolate" "Name")
        FROM Person
        WHERE "LikesChocolate")
      '(("LikesChocolate" "Name")
        (#t "David")
        (#t "Jen")))

; Condition as function of one attribute, select all
(test (SELECT *
        FROM Person
        WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

; Condition as function of one attribute, select none
(test (SELECT '()
        FROM Teaching
        WHERE (equal? "Name" "David"))
      '(()
        ()
        ()))

; Constant true condition
(test (SELECT *
        FROM Person
        WHERE #t)
      Person)

; Constant false compound condition
(test (SELECT *
        FROM Person
        WHERE (> (string-length "David") 20))
      '(("Name" "Age" "LikesChocolate")))

; Condition on a literal table
(test (SELECT '("C" "B")
        FROM '(("A" "B" "C") 
               (1 2 3)
               (3 10 40)
               (4 4 4)
               (2 3 -1))
        WHERE (odd? "A"))
      '(("C" "B")
        (3 2)
        (40 10)))

; Simple condition on joined tables
(test (SELECT *
        FROM [Person "P"] [Teaching "T"]
        WHERE (equal? "P.Name" "T.Name"))
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "David" "CSC343")
        ("Paul" 100 #f "Paul" "CSC108")))

; Compound condition on three joined tables
(test (SELECT '("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        FROM [Person "P"] [Teaching "T"] [Person "P1"]
        WHERE (And "P.LikesChocolate" (equal? "P1.Name" "T.Name")))
      '(("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        ("David" #t 20 "CSC324")
        ("Paul" #f 20 "CSC108")
        ("David" #t 20 "CSC343")
        ("David" #t 30 "CSC324")
        ("Paul" #f 30 "CSC108")
        ("David" #t 30 "CSC343")))


;-------------------------
;New Tests defined by me.

;Check that the join with an empty table produces the same result.
(test (SELECT * 
              FROM [Person "P"] [Empty "E"]
              WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

;Predicates in the WHERE clause that do not incorporate any attributes.
(test (SELECT *
        FROM Person
        WHERE "Chocolate") ; Chocolate represents any attribute that does not exist in the table.
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Jen" 30 #t)
        ("Paul" 100 #f)))

;Testing the WHERE clause on a table with duplicate tuples.
(test (SELECT *
        FROM Duplicate
        WHERE "LikesChocolate") 
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("David" 20 #t)))

;-------------------------

; ---- ORDER BY ----
; Order by attribute
(test (SELECT *
        FROM Person
        ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order by attribute, not selected
(test (SELECT '("Name")
        FROM Person
        ORDER BY "Age")
      '(("Name")
        ("Paul")
        ("Jen")
        ("David")))

; Order by a function of an attribute
(test (SELECT *
        FROM Person
        ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Paul" 100 #f)
        ("Jen" 30 #t)))

; Order with duplicate
(test (SELECT *
        FROM Teaching
        ORDER BY (+ (string-length "Name") (string-length "Course")))
      '(("Name" "Course")
        ("David" "CSC324")
        ("David" "CSC343")
        ("Paul" "CSC108")))

; Order on a literal table
(test (SELECT *
        FROM '(("A" "B" "C") 
               (1 2 3)
               (3 10 40)
               (4 4 4)
               (2 3 -1))
        ORDER BY "C")
      '(("A" "B" "C")
        (3 10 40)
        (4 4 4)
        (1 2 3)
        (2 3 -1)))

; Order on two tables
(test (SELECT *
        FROM [Person "P"] [Teaching "T"]
        ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #t "David" "CSC343")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")))

;-------------------------
;New Tests defined by me.

; Make sure that ORDER BY does not fail with an empty table.
(test (SELECT *
              FROM Empty
              ORDER BY "Age")
      '(()
        ()))

; Make sure that ORDER BY works when joining with an empty table.
(test (SELECT *
              FROM [Person "P"] [Empty "E"]
              ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order on three tables
(test (SELECT *
        FROM [Person "P"] [Teaching "T"] [OneTuple "O"]
        ORDER BY "P.Age")
      '(("P.Name" "P.Age" "P.LikesChocolate" "T.Name" "Course" "O.Name" "O.Age" "O.LikesChocolate")
        ("Paul" 100 #f "David" "CSC324" "David" 20 #t)
        ("Paul" 100 #f "Paul" "CSC108" "David" 20 #t)
        ("Paul" 100 #f "David" "CSC343" "David" 20 #t)
        ("Jen" 30 #t "David" "CSC324" "David" 20 #t)
        ("Jen" 30 #t "Paul" "CSC108" "David" 20 #t)
        ("Jen" 30 #t "David" "CSC343" "David" 20 #t)
        ("David" 20 #t "David" "CSC324" "David" 20 #t)
        ("David" 20 #t "Paul" "CSC108" "David" 20 #t)
        ("David" 20 #t "David" "CSC343" "David" 20 #t)))

;-------------------------


; ---- ORDER BY and WHERE ----
; Use attributes, select all 
(test
 (SELECT * 
   FROM Person 
   WHERE "LikesChocolate" 
   ORDER BY "Age")
 '(("Name" "Age" "LikesChocolate")
   ("Jen" 30 #t)
   ("David" 20 #t)))

; Use attributes, select one unused attribute
(test
 (SELECT '("Name") 
   FROM Person 
   WHERE "LikesChocolate" 
   ORDER BY "Age")
 '(("Name")
   ("Jen")
   ("David")))

; Two joined tables, select all
(test
 (SELECT * 
   FROM [Person "P"] [Teaching "T"] 
   WHERE (equal? "P.Name" "T.Name")
   ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Two joined tables, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
   FROM [Person "P"] [Teaching "T"] 
   WHERE (equal? "P.Name" "T.Name")
   ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))


;-------------------------
;New Tests defined by me.

; Make sure that WHERE and ORDER BY don't fail with the Empty table included in the FROM clause.
(test
 (SELECT * 
   FROM [Person "P"] [Teaching "T"] [Empty "E"] 
   WHERE (equal? "P.Name" "T.Name")
   ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Make sure that WHERE and ORDER BY don't fail with the Empty table included in the FROM clause.
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
   FROM [Person "P"] [Teaching "T"] [Empty "E"] 
   WHERE (equal? "P.Name" "T.Name")
   ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))

; Make sure that WHERE and ORDER BY work when selecting one attribute from a 3 table join and ordering by an attribute that will have the same values and will not be displayed.
(test
 (SELECT '("P.Name")
   FROM [Person "P"] [Teaching "T"] [OneTuple "O"] 
   WHERE (And (equal? "P.Name" "T.Name") (equal? "P.Name" "O.Name"))
   ORDER BY "P.Age")
 '(("P.Name")
   ("David")
   ("David")))

;-------------------------


; ---- Nested queries ----
(test
 (SELECT * 
   FROM (SELECT '("Age" "Name") FROM Person))
 '(("Age" "Name")
   (20 "David")
   (30 "Jen")
   (100 "Paul")))

(test
 (SELECT '("Person.Name" "Course")
   FROM [(SELECT '("Name") FROM Person) "Person"]
        [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                           (equal? "Course" "CSC108")))
         "Teaching"])
 '(("Person.Name" "Course")
   ("David" "CSC108")
   ("David" "CSC343")
   ("Jen" "CSC108")
   ("Jen" "CSC343")
   ("Paul" "CSC108")
   ("Paul" "CSC343")))

; Nested query containing a literal
(test
 (SELECT *
   FROM [(SELECT '("A") 
           FROM '(("A" "B") 
                  (1)
                  (10)))
         "Table1"]
        [(SELECT *
           FROM '(("C" "A")
                  ("Hi" "Bye")
                  ("Dog" "Cat")
                  ("Red" "Blue")))
         "Table2"]
   WHERE (And (equal? (string-length "Table2.A") 3) (< 0  "Table1.A")))
 '(("Table1.A" "C" "Table2.A")
   (1 "Hi" "Bye")
   (1 "Dog" "Cat")
   (10 "Hi" "Bye")
   (10 "Dog" "Cat")))
