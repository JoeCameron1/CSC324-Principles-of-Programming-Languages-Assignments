#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***

Joseph Cameron, camer249, 1003699081

|#

; General style of variable names:
; Functions: Function-Name (Using the dash)
; Function attributes: Attribute_Name (Using the underscore)

; General Style of Code:
; Functions will be laid out in a java-like format, where brackets mimic the semi-colon format of java methods.

#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
;(define (And x y) (and x y)) 
;(define (Or x y) (or x y))
;(define (If x y z) (if x y z))
;
; Correction Oct 5 2016
(define-syntax If
  (syntax-rules ()
  ((If a b c)
  (if a b c))))

(define-syntax And
  (syntax-rules ()
    [(And p q)
     (if (not p)
         #f
         q)]))

(define-syntax Or
  (syntax-rules ()
    [(Or p q)
     (if p
         #t
         q)]))

; Please do define And, Or as syntactic forms
; We have actually done this in class you may use the class code and this week's lab code for this.
  

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         SELECT)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define attributes car)

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define tuples cdr)

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  (length (tuples table)))


; Part I "WHERE" helpers; you may or may not wish to implement these.

(define Person
  '(("Name" "Age" "LikesChocolate")
    ("David" 20 #t)
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")))

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Basic selection (SELECT <attrs> FROM <table>) is represented by the functions find-index, attribute-value, many-attribute-values and table-attribute-values.

; find-index: Finds the index in the list for a certain attribute, is a very important helper function.
; attribute-value: Finds the value for an attribute.
; many-attribute-values: Uses attribute-value to find the values for more than one attribute.
; table-attribute-values: Uses many-attribute-values and attribute-value to find values for attributes within a table.

#|
NEW HELPER FUNCTION find_index:

Returns the index of an element in a list.
|#

(define (find-index element list1)
  (if (empty? list1)
      -1
      (if (equal? element (first list1))
          0
          (if (= (find-index element (rest list1)) -1) ; This check will prevent elements not present in the list to obtaining the last index. DELETE: = or equal?
              -1
              (+ 1 (find-index element (rest list1)))
              )
          )
      )
  )

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#

(define (attribute-value attribute_list attribute tuple)
  (if (= (find-index attribute attribute_list) -1) ; Calling the find-index function, which is my helper function above.
      "Attribute does not exist."
      (list-ref tuple (find-index attribute attribute_list)) ; Calling the find-index function, which is my helper function above.
      )
  )

#|
NEW HELPER FUNCTION many-attribute-values:

A function that takes: 
  - a list of attributes
  - a list of strings representing attributes.
  - a tuple

  and returns the values of the tuple corresponding to the specified attributes.

Uses attribute-value as a helper function.

|#

(define (many-attribute-values attribute_list attributes tuple)
  (map (lambda (attribute) (attribute-value attribute_list attribute tuple)) attributes)
  )

#|
NEW HELPER FUNCTION table-attribute-values:

A function that takes: 
  - a list of strings representing attributes.
  - a tuple

  and returns the values of the tuple corresponding to the specified attributes within the table.

Uses many-attribute-values as a helper function.

|#
; Must use attiribute_s instead of attributes to not confuse with the function attributes.

(define (table-attribute-values attribute_s table)
  (cons attribute_s (map (lambda (tuple) (many-attribute-values (attributes table) attribute_s tuple)) (tuples table)))
  )

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Multiple tables (SELECT <attrs> FROM [<table1> <name1>] [<table2> <name2>] ...) is represented by the functions cartesian-product, join, same-attribute, name-formation,
; further-name-formation and join-table-columns.

; cartesian-product: Returns the cartesian product of 2 lists.
; join: Uses cartesian-product to join 2 or more tables together.
; same-attribute: Determines whether an attribute appears multiple times. Used for the Table.attribute format.
; name-formation: Produces the Table.attribute format for single tables.
; further-name-formation: Produces the Table.attribute format for multiple tables.
; join-table-columns: Adds the formatted column names from 'further-name-formation' to the joined table produced by the helper function 'join'.

#|
NEW HELPER FUNCTION cartesian-product:

A function that takes: 
  - a list of tuples from table 1.
  - a list of tuples from table 2.

  and returns the cartesian product of both lists.

|#

(define (cartesian-product tuples1 tuples2)
  (if (or (empty? tuples1) (empty? tuples2))
      '()
      (append (map (lambda (x) (append (first tuples1) x)) tuples2) (cartesian-product (rest tuples1) tuples2))
      )
  )

#|
NEW HELPER FUNCTION join:

A function that takes: 
  - a list of tables to be joined.

  and returns the cartesian product of the elements within the list.

Allows more than 2 tables to be joined.
Uses cartesian-product as a helper function.

|#

(define (join tables)
  (if (empty? (rest tables))
      (first tables)
      (cartesian-product (first tables) (join (rest tables)))
      )
  )

#|
NEW HELPER FUNCTION same-attribute:

A function that takes: 
  - a string referring to an attribute.
  - a list of attributes

  and returns the boolean result of whether the attribute appears many times in the list.

|#

(define (same-attribute attribute attribute_list)
  (> (length (filter (lambda (x) (equal? attribute x)) (apply append attribute_list))) 1)
  )

#|
NEW HELPER FUNCTION name-formation:

A function that takes: 
  - a list of wanted attributes.
  - a list of attributes from a table.
  - a string representing the name of a table.

  and returns the Table.attribute format that is desired.

Uses same-attribute as a helper function to determine whether there are two attributes of the same name.

|#

(define (name-formation wanted_attribute_list attribute_list name)
  (define (append-name attribute)
    (if (same-attribute attribute attribute_list)
        (string-append name "." attribute)
        attribute
        )
    )
  (map append-name wanted_attribute_list)
  )

#|
NEW HELPER FUNCTION further-name-formation:

A function that takes: 
  - a list of attributes.
  - a list of table names.

  and returns the final Table.attribute format that is desired.

Uses name-formation as a helper function to build each individual Table.attribute column name if it is required.

|#

(define (further-name-formation attribute_list1 table_names1)
  (define (further-name-formation2 attribute_list2 attribute_list3 table_names2)
    (if (empty? attribute_list2)
        '()
        (cons (name-formation (first attribute_list2) attribute_list3 (first table_names2)) (further-name-formation2 (rest attribute_list2) attribute_list3 (rest table_names2)))
        )
    )
  (apply append (further-name-formation2 attribute_list1 attribute_list1 table_names1))
  )

#|
NEW HELPER FUNCTION join-table-columns:

A function that takes: 
  - a list of table names.

  and returns the final Table.attribute column names that are desired attached to the table of tuples created from 'join'.

Uses further-name-formation and join as helper functions.

|#

(define (join-table-columns tables1)
  (let* ([tables2 (map car tables1)]
         [information (join (map tuples tables2))]
         [attributeList (further-name-formation (map attributes tables2) (map cdr tables1))]
         )
    (cons attributeList information))
  )

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Filtering tuples (... WHERE <pred>) represented by the functions table-satisfy-function and replace-attr.

; table-satisfy-function: Returns a new table containing only tuples that satisfy a particular function.
; replace-attr: Uses the function 'find-index' to check if a value is an attribute, if it is it returns the corresponding value in a tuple, if not it returns the attribute.

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#


(define (table-satisfy-function f table)
  (cons (attributes table) (filter f (tuples table)))
  )

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#

(define (replace-attr x attribute_list)
  (lambda (tuple)
    (if (= (find-index x attribute_list) -1)
        x
        (attribute-value attribute_list x tuple)
        )
    )
  )

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; ORDER BY (... ORDER BY <ord>) represented by is_Larger and sort-table.

; is_Larger: Checks whether integers or strings are of greater value in comparison to another int/string.
; sort-table: Sorts the table.

#|
NEW HELPER FUNCTION is_Larger:

A function that takes: 
  - a string/integer.
  - a string/integer.

  and returns whether one is greater than the other in boolean form.

|#

(define (is_Larger x y)
  (if (string? x)
      (string>? x y)
      (> x y)
      )
  )

#|
NEW HELPER FUNCTION sort-table:

A function that takes: 
  - sorting specification.
  - a table.

  and returns a sorted table that conforms to the sorting specification, where the sorting specification would be is_Larger.

|#

(define (sort-table key_function table)
  (cons (attributes table) (sort (tuples table) is_Larger #:key key_function))
  )

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Starter for Part 3; feel free to ignore!

; What should this macro do?
;(define-syntax replace
;  (syntax-rules ()
;    ; The recursive step, when given a compound expression
;    [(replace (expr ...) table)
;     ; Change this!
;     (void)]
;    ; The base case, when given just an atom. This is easier!
;    [(replace atom table)
;     ; Change this!
;     (void)]))

; Part 3: Macros

;replace / Used in WhereClause
(define-syntax replace
  (syntax-rules ()
    [(replace (expr ...) table)
     (lambda (tuple)
       (if (empty? tuple)
           tuple
           (let ([expr2 (list ((replace expr table) tuple) ...)]) (apply (first expr2) (rest expr2)))
           )
       )
     ]
    [(replace atom table)
     (lambda (tuple)
       (if (empty? tuple)
           tuple
           ((replace-attr atom table) tuple)
           )
       )
     ]
    )
  )

;FormatColumns / FROM
(define-syntax FormatColumns
  (syntax-rules ()
    [(FormatColumns [<table> <name>]) (list (cons <table> <name>))]
    [(FormatColumns <parameter> ...) (append (FormatColumns <parameter>) ...)]
    )
  )

;WhereClause / WHERE
(define-syntax WhereClause
  (syntax-rules ()
    [(WhereClause <expr> <table>) (table-satisfy-function (replace <expr> (attributes <table>)) <table>)]
    )
  )

;OrderClause / ORDER BY
(define-syntax OrderClause
  (syntax-rules ()
    [(OrderClause <expr> <table>) (sort-table (replace <expr> (attributes <table>)) <table>)]
    )
  )

;SELECT
(define-syntax SELECT
  (syntax-rules (* FROM WHERE ORDER BY)
    [(SELECT <attributes> FROM <table> ... WHERE <condition> ORDER BY <order>)
     (SELECT <attributes> FROM (OrderClause <order> (WhereClause <condition> (SELECT * FROM <table> ...))))]
    [(SELECT <attributes> FROM <table> ... ORDER BY <order>)
     (SELECT <attributes> FROM (OrderClause <order> (SELECT * FROM <table> ...)))]
    [(SELECT <attributes> FROM <table> ... WHERE <condition>)
     (SELECT <attributes> FROM (WhereClause <condition> (SELECT * FROM <table> ...)))]
    [(SELECT * FROM <table>)
     <table>]
    [(SELECT <attributes> FROM <table>)
     (table-attribute-values <attributes> <table>)]
    [(SELECT * FROM <table1> <table2> ...)
     (join-table-columns (FormatColumns <table1> <table2> ...))]    
    [(SELECT <attributes> FROM <table1> <table2> ...)
     (SELECT <attributes> FROM (join-table-columns (FormatColumns <table1> <table2> ...)))]
    )
  )
