#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 4
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#
(define (add-to-all lst item)
  (if (null? lst)
      empty
      (cons (cons item (car lst))
            (add-to-all (cdr lst) item))))
; subsets

(define (subsetValues lst)
  (if (empty? lst)
      '(())
      (let ((rst (subsetValues (cdr lst))))
        (append (add-to-all rst (car lst))
                rst))))

(define (subsets l)
  (add-value (subsetValues l)))

(define (add-value lst)
  (if (equal? 1 (length lst))
      (first lst)
      (-<  (first lst) (add-value (rest lst)))))

; QUESTION 5
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
;This is what combines each different row with one another.
(define (cartesian-product table1 table2)
  (if (or (empty? table1) (empty? table2))
      '()
      (append (map (lambda (x) (append (first table1) x)) table2) (cartesian-product (rest table1) table2))))

;Used to find all permutations of each row.
(define (permutations s)
  (cond [(empty? s) empty]
        [(empty? (rest s)) (list s)]
        [else
         (let splice [(l '()) (m (first s)) (r (rest s))]
           (append
            (map (lambda (x) (cons m x))
                 (permutations (append l r)))
            (if (empty? r)
                empty
                (splice (cons m l) (first r) (rest r)))))]))

;What calls all the functions to get the result.
(define (sudoku-4 puzzle)
  (let ([answer (valid-sudoku (replace-all-blanks puzzle))])
    (if (empty? answer)
        "false."
        (add-value answer))))

;This function checks the columns and makes sure they are correct.
(define (validColumns puzzle)
  (if
   (and
    (not (equal? (first (first puzzle)) (first (second puzzle))))
    (not (equal? (first (first puzzle)) (first (third puzzle))))
    (not (equal? (first (first puzzle)) (first (fourth puzzle))))
    (not (equal? (first (second puzzle)) (first (third puzzle))))
    (not (equal? (first (second puzzle)) (first (fourth puzzle))))
    (not (equal? (first (third puzzle)) (first (fourth puzzle))))
    
    (not (equal? (second (first puzzle)) (second (second puzzle))))
    (not (equal? (second (first puzzle)) (second (third puzzle))))
    (not (equal? (second (first puzzle)) (second (fourth puzzle))))
    (not (equal? (second (second puzzle)) (second (third puzzle))))
    (not (equal? (second (second puzzle)) (second (fourth puzzle))))
    (not (equal? (second (third puzzle)) (second (fourth puzzle))))
    
    (not (equal? (third (first puzzle)) (third (second puzzle))))
    (not (equal? (third (first puzzle)) (third (third puzzle))))
    (not (equal? (third (first puzzle)) (third (fourth puzzle))))
    (not (equal? (third (second puzzle)) (third (third puzzle))))
    (not (equal? (third (second puzzle)) (third (fourth puzzle))))
    (not (equal? (third (third puzzle)) (third (fourth puzzle))))
    
    (not (equal? (fourth (first puzzle)) (fourth (second puzzle))))
    (not (equal? (fourth (first puzzle)) (fourth (third puzzle))))
    (not (equal? (fourth (first puzzle)) (fourth (fourth puzzle))))
    (not (equal? (fourth (second puzzle)) (fourth (third puzzle))))
    (not (equal? (fourth (second puzzle)) (fourth (fourth puzzle))))
    (not (equal? (fourth (third puzzle)) (fourth (fourth puzzle)))))
   true
   false))

;This function checks the squares and makes sure they are correct.
(define (validSquares puzzle)
  (if
   (and
    ;Top Left
    (not (equal? (first (first puzzle)) (first (second puzzle))))
    (not (equal? (first (first puzzle)) (second (second puzzle))))
    (not (equal? (second (first puzzle)) (first (second puzzle))))
    (not (equal? (second (first puzzle)) (second (second puzzle))))
    
    ;Top Right
    (not (equal? (third (first puzzle)) (third (second puzzle))))
    (not (equal? (third (first puzzle)) (fourth (second puzzle))))
    (not (equal? (fourth (first puzzle)) (third (second puzzle))))
    (not (equal? (fourth (first puzzle)) (fourth (second puzzle))))
    
    ;Bottom Left
    (not (equal? (first (third puzzle)) (first (fourth puzzle))))
    (not (equal? (first (third puzzle)) (second (fourth puzzle))))
    (not (equal? (second (third puzzle)) (first (fourth puzzle))))
    (not (equal? (second (third puzzle)) (second (fourth puzzle))))
    
    ;Bottom Right
    (not (equal? (third (third puzzle)) (third (fourth puzzle))))
    (not (equal? (third (third puzzle)) (fourth (fourth puzzle))))
    (not (equal? (fourth (third puzzle)) (third (fourth puzzle))))
    (not (equal? (fourth (third puzzle)) (fourth (fourth puzzle))))
    )
   true
   false))

#|
This function makes sure that the sudoku is valid, therefore, the columns and
squares of the sudoku 4 x 4 puzzle are correct.
|#
(define (valid-sudoku l)
  (let ([validColumnPuzzles '()]
        [validPuzzles '()])
    (map (lambda (i)
           (cond
             [(validColumns i) (set! validColumnPuzzles (append validColumnPuzzles (list i)))])) l)
    (map (lambda (i)
           (cond
             [(validSquares i) (set! validPuzzles (append validPuzzles (list i)))]))
         validColumnPuzzles)
    
    validPuzzles))

#|
This function subtracts values from one list
from another. This is used when we want to see
what values have already been used in a row.
Ex:
(subtract '(1 2 3 4) '("" "" 1 "")) --> '(2 3 4)
|#
(define (subtract xs ys)
  (if (empty? ys)
      xs
      (subtract (remove (first ys) xs) (rest ys))))



#|
This function allows you to insert a the value x
into any position (pos) in the list (lst) and returns
the new list with the value added back.
|#

(define (insert-at lst pos x)
  (define-values (before after) (split-at lst pos))
  (append before (cons x after)))

#|
Takes in a row (a list) in the table,
rowNum, the location of the row, and the
puzzle itself.

It then appends the puzzle's row at that location
if the list 'row' contains no values. Since we know
then that the row in puzzle is completley full,
and has no blank values.
|#
(define (fullRow row rowNum puzzle)
  (if (empty? row)
      (set! answer (append answer (list (list-ref puzzle rowNum))))
      (set! answer (append answer (list (list))))))

(define (makeRow vals rowNum puzzle)
  (let ([currRow 0]
        [currVal 0]
        [valsLoc 0]
        [tempRow (list)])
    (map (lambda (i)
           (map (lambda (x)
                  (cond
                    [(equal? currRow rowNum)
                     (if(string? x)
                        (set! tempRow (append tempRow (list (list-ref vals valsLoc))))
                        (set! tempRow (append tempRow (list x))))])
                  (cond
                    [(and (equal? currRow rowNum) (string? x)) (set! valsLoc (+ valsLoc 1))])
                  (set! currVal (+ currVal 1))) i)
           (set! currRow (+ currRow 1))
           (set! currVal 0)) puzzle)
    tempRow))

#|
This function is what finds every possible permutation of each row.
|#
(define (replace-all-blanks puzzle)
  (let ([validValues (list 1 2 3 4)]
        [row1 (list)]
        [row2 (list)]
        [row3 (list)]
        [row4 (list)]
        [allRow1 (list)]
        [allRow2 (list)]
        [allRow3 (list)]
        [allRow4 (list)]
        [rowNum 1]
        [allRows '()]
        [tempRow '()])
    (map (lambda (i)
           (cond
             [(equal? rowNum 1) (set! row1 (append row1 (spaces i)))]
             [(equal? rowNum 2) (set! row2 (append row2 (spaces i)))]
             [(equal? rowNum 3) (set! row3 (append row3 (spaces i)))]
             [(equal? rowNum 4) (set! row4 (append row4 (spaces i)))])
           (set! rowNum (+ rowNum 1))) puzzle)
    
    (fullRow row1 0 puzzle)
    (fullRow row2 1 puzzle)
    (fullRow row3 2 puzzle)
    (fullRow row4 3 puzzle)
    
    (set! allRows (append (list row1) (list row2) (list row3) (list row4)))
    (set! rowNum 0)
    (map (lambda (i)
           (if (empty? i)
               (cond
                 [(equal? rowNum 0) (set! allRow1 (list (list (list-ref answer rowNum))))]
                 [(equal? rowNum 1) (set! allRow2 (list (list (list-ref answer rowNum))))]
                 [(equal? rowNum 2) (set! allRow3 (list (list (list-ref answer rowNum))))]
                 [(equal? rowNum 3) (set! allRow4 (list (list (list-ref answer rowNum))))])
               (map (lambda (i)
                      (cond
                        [(equal? rowNum 0) (set! allRow1 (append allRow1 (list (list (makeRow i rowNum puzzle)))))]
                        [(equal? rowNum 1) (set! allRow2 (append allRow2 (list (list (makeRow i rowNum puzzle)))))]
                        [(equal? rowNum 2) (set! allRow3 (append allRow3 (list (list (makeRow i rowNum puzzle)))))]
                        [(equal? rowNum 3) (set! allRow4 (append allRow4 (list (list (makeRow i rowNum puzzle)))))]))
                    (permutations (subtract validValues (list-ref puzzle rowNum))))
               )          
           (set! rowNum (+ rowNum 1))) allRows)
    (cartesian-product (cartesian-product (cartesian-product allRow1 allRow2) allRow3) allRow4)))

#|
Finds all the spaces in each row and returns their locations.
|#
(define (spaces lst)
  (let ([stringLoc '()]
        [loc 0])
    (map (lambda (i)
           (cond
             [(string? i) (set! stringLoc (append stringLoc (list loc)))])
           (set! loc (+ loc 1))) lst)
    stringLoc))

;Holds rows that have no empty spaces.
(define answer
  (list))


; QUESTION 6
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< combine init expr)
     (foldl combine init (all expr))]))