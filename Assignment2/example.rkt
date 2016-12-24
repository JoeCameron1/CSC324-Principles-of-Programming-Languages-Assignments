
#lang racket


(define board '(
  (0 0 0 0)
  (0 1 0 0)
  (0 0 2 0)
  (2 0 3 0)))
;this is a board that is unsolved and the function 'ss' will return the solution to this 4x4 sudoku board.

(define (build-list l1 l2 l3)
  (append l1 (append l2 l3))
  )
;pre: takes 3 lists l1,l2,l3
;post: returns 1 list contianing all items in l1 l2 and l3 
;why: this function was made to combine the non-candidates from column row and quadrant that intersect with the current cell we want to change

(define (get-row cell)
  (car cell)
  )
;pre: takes a list containing row and column e.g. '(3 1) meaning row 3 column 1
;post: returns the car of the list.
;why: to easily get the row that intersects with the cell we wish to change.

(define (get-column cell)
  (car (cdr cell))
  )
;pre: takes a list containing row and column e.g. '(3 1) meaning row 3 column 1
;post: returns the car of the cdr of the list.
;why: This function is to easily get the column that intersects with the cell we wish to change.


(define (row-ref board cell)
  (list-ref board (get-row cell))
  )
;pre: takes a board and a cell.
;post: returns the list representation of the row that intersects the cell.
;why: to easily find the non-candidates in that row.

(define (get-quad cell)
  (cond
    ((and (< (get-row cell) 2) (< (get-column cell) 2)) 0)
    ((and (< (get-row cell) 2) (>= (get-column cell) 2)) 1)
    ((and (>= (get-row cell) 2) (< (get-column cell) 2)) 2)
    (else 3)
    )
  )
;pre: takes a list containing row and column e.g. '(3 1) meaning row 3 column 1.
;post: returns a number that represents the quadrant the cell is in.
;why: to easily get the quadrant that the cell intersects with.


(define (quad-ref board cell)
  (define quad (get-quad cell))

  (cond
    ((= quad 0) (list (list-ref (list-ref board 0) 0)
                      (list-ref (list-ref board 0) 1)
                      (list-ref (list-ref board 1) 0)
                      (list-ref (list-ref board 1) 1)))
                
    ((= quad 1) (list (list-ref (list-ref board 0) 2)
                      (list-ref (list-ref board 0) 3)
                      (list-ref (list-ref board 1) 2)
                      (list-ref (list-ref board 1) 3)))
                
    ((= quad 2) (list (list-ref (list-ref board 2) 0)
                      (list-ref (list-ref board 2) 1)
                      (list-ref (list-ref board 3) 0)
                      (list-ref (list-ref board 3) 1)))
                
    (else (list       (list-ref (list-ref board 2) 2)
                      (list-ref (list-ref board 2) 3)
                      (list-ref (list-ref board 3) 2)
                      (list-ref (list-ref board 3) 3)))
    )
  )
;pre: it takes two lists that represents a board and a cell
;post: returns a list representation of the quadrant that intersects with the cell
;why: to easily find the non-candidates in that quadrant.

(define (column-ref board cell)
  (list (list-ref (list-ref board 0) (get-column cell))
        (list-ref (list-ref board 1) (get-column cell))
        (list-ref (list-ref board 2) (get-column cell))
        (list-ref (list-ref board 3) (get-column cell)))
  )
;pre: it takes two lists that represents a board and a cell.
;post: returns the list representation of the column that intersects the cell.
;why: to easily find the non-candidates in that column.


(define (compare-to-numset LON)
  (if (empty? LON)
      '(1 2 3 4)
      (remove (car LON) (compare-to-numset (cdr LON)))
      )
  )

;pre: it takes a list of non-candidates e.g. '(1 3 4)
;post: returns the list of candidates e.g '(2)
;why: the list of candidates will be used to test each cadidate in a cell.


(define (eliminate-duplicate lst)
  (cond
    ((null? lst) '())
    ((member (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
    (else (cons (car lst) (remove-duplicates (cdr lst))))
    )
  )

;pre: it takes a list of numbers, lst
;post: returns the a list containing all number of lst with no repetition
;why: because the row, column, and quadrant that intersect with a cell, may have the same numbers. this will end with non candidates that repeat.

(define (clear-zero lst)
  (cond
    ((empty? lst) '())
    ((= 0 (car lst)) (clear-zero (cdr lst)))
    (else (cons (car lst) (clear-zero (cdr lst))))
    )
  )
;pre: takes a list of numbers, lst
;post: returns a list containing the numbers in lst, excluding zeros
;why: Zero is never a candidate so we do not need to check if it is.

(define (check-column board cell)
  (clear-zero (column-ref board cell))
  )

;pre: it takes two lists that are a board and a cell
;post: returns the non-candidates of that cell in the row that intersects with the cell
;why: eventually to find candidates by comparing the non-candidates with '(1 2 3 4)

(define (check-row board cell)
 (clear-zero (row-ref board cell))
)
;pre: it takes two lists that are a board and a cell
;post: returns the non-candidates of that cell in the column that intersects with the cell
;why: eventually to find candidates by comparing the non-candidates with '(1 2 3 4)

(define (check-quad board cell)
  (clear-zero (quad-ref board cell))
  )
;pre: it takes two lists that are a board and a cell
;post: returns the non-candidates of that cell in the quadrant that intersects with the cell
;why: eventually to find candidates by comparing the non-candidates with '(1 2 3 4)


(define (get-candidates board cell)
  (define LON (build-list (check-column board cell) (check-row board cell) (check-quad board cell)))
  (define cleanLON (eliminate-duplicate LON))
  (compare-to-numset cleanLON)
  )
;pre: it takes two lists that are a board and a cell
;post: returns the possible candidates that can go into that cell
;why: to eventually test the candidates in that cell.

(define (update-board board cell item)
  (list-set board (get-row cell)(list-set (list-ref board (get-row cell)) (get-column cell) item))
  )
;pre: it takes two lists that are a board and a cell. it also takes a number
;post: returns a copy of the board with the number in the specified cell
;why: this is where we test the candidates. each time we update the board, we insert one of the candidates into the cell and move to the next cell.


(define (next cell)
  (if (= (get-column cell) 3)
      (list (+ 1 (get-row cell)) 0)
      (list (get-row cell) (+ 1 (get-column cell)))
      )
  )
;pre: it takes a list that represnts cell as input.
;post: returns the next cell we will test. example '(0 0) will be followed with ('0 1) and '(0 3) followed by '(1 0).
;why: to easily move across the board.

(define (test-all candidates board cell);
  (cond ((empty? candidates) (list #f board))
        ((car (ss (update-board board cell (car candidates)) (next cell))) (ss (update-board board cell (car candidates)) (next cell)))
        (else (test-all (cdr candidates) board cell))))
;pre: takes a list of candidates, a board and a cell
;post: returns a list containing a boolean and a board e.g. '(#t board)
;if the list of candidates is empty it will return '(#f board)
;if one of the candidates leads to a solved board, then '(#t solved_board)
;why: to test each individual candidate. we recursively go through each candidate and check if that candidate was correct for the position
; if the candidate was correct for the position, then the board will ultimately be solved.
; if the candidate was put there incorrectly, then we simply test the next candidate.
; if we exhaust all candidates, then we simply return (#f board) the funciton that receives that, will test the next avialable candidate
; so on and so forth. until there is a solution.

(define (get-item board cell)
  (list-ref (row-ref board cell) (get-column cell))
  )
;pre: it takes two lists that represent a board and a cell.
;post: the function returns the number that currently in the cell. 
;why? This function is used to help us find the empty cells in the board. 

(define (ss board cell)
  (cond
    ((= (get-row cell) 4) (list #t board))
    ((not (= 0 (get-item board cell))) (ss board (next cell)))
    ((empty? (get-candidates board cell)) (list #f board))
    (else
     (test-all (get-candidates board cell) board cell)
     )
    )
  )

(define (solve board)
  (cadr (ss board '(0 0)))
  )
;precondition: this takes a two lists, the unsolved sudoku board and a cell that represents '(0,0).
;Whereas, the cell '(0,0) represents the beginning of the board.
;postcondition: this will return a list that represents a 4x4 sudoku board


(define (draw-board board)
  (display (row-ref board '(0 0)))
  (display "\n")
  (display (row-ref board '(1 1)))
  (display "\n")
  (display (row-ref board '(2 2)))
  (display "\n")
  (display (row-ref board '(3 3)))
  (display "\n")
)


(draw-board (cadr(ss board '(0 0))))
;pre: it takes a list within a list that represents the board
;post: it will display the sudoku board representation
;why? I created this function to draw the sudoku board correctly


(define solvedboards '(
 (((3 0 0 1)
  (1 0 0 0)
  (0 0 0 0)
  (0 0 2 0))

   ((3 2 4 1)
  (1 4 3 2)
  (2 3 1 4)
  (4 1 2 3)));1

  (((4 0 0 0)
  (0 0 4 0)
  (0 0 0 3)
  (0 2 0 0))

   ((4 1 3 2)
  (2 3 4 1)
  (1 4 2 3)
  (3 2 1 4)));2

   (((0 0 1 0)
  (3 0 0 0)
  (0 3 0 0)
  (0 0 0 4))

   ((2 4 1 3)
  (3 1 4 2)
  (4 3 2 1)
  (1 2 3 4)));3

   (((0 0 4 0)
  (1 0 0 0)
  (0 2 0 0)
  (0 0 0 3))

   ((2 3 4 1)
  (1 4 3 2)
  (3 2 1 4)
  (4 1 2 3)));4

   (((0 0 4 0)
  (1 0 0 0)
  (0 2 0 0)
  (0 0 0 3))

   ((2 3 4 1)
  (1 4 3 2)
  (3 2 1 4)
  (4 1 2 3)));5

   (((2 0 0 0)
  (0 0 0 3)
  (0 0 0 0)
  (0 4 1 0))

   ((2 3 4 1)
  (4 1 2 3)
  (1 2 3 4)
  (3 4 1 2)));6

  (((0 0 1 0)
  (0 4 0 0)
  (2 0 0 0)
  (0 0 0 1))

   ((3 2 1 4)
  (1 4 3 2)
  (2 1 4 3)
  (4 3 2 1)));7

  (((0 0 0 3)
  (0 3 0 0)
  (4 0 0 0)
  (0 0 1 0))

   ((1 4 2 3)
  (2 3 4 1)
  (4 1 3 2)
  (3 2 1 4)));8

  (((0 2 0 0)
  (0 0 2 0)
  (0 0 0 3)
  (4 0 0 0))

   ((1 2 3 4)
  (3 4 2 1)
  (2 1 4 3)
  (4 3 1 2)));9

  (((2 0 0 0)
  (0 0 0 3)
  (0 2 0 0)
  (0 0 1 0))

   ((2 3 4 1)
  (4 1 2 3)
  (1 2 3 4)
  (3 4 1 2)));10

   (((0 4 0 0)
  (0 0 0 2)
  (0 0 0 0)
  (0 0 2 3))

   ((2 4 3 1)
  (1 3 4 2)
  (3 2 1 4)
  (4 1 2 3)));11

  (((0 4 0 0)
  (3 0 0 0)
  (0 0 1 4)
  (0 0 2 0))

   ((1 4 3 2)
  (3 2 4 1)
  (2 3 1 4)
  (4 1 2 3)));12


  (((0 0 0 3)
  (0 0 0 4)
  (3 0 0 0)
  (2 0 0 0))

   ((4 2 1 3)
  (1 3 2 4)
  (3 1 4 2)
  (2 4 3 1)));13


   (((1 0 0 0)
  (0 0 0 1)
  (0 4 0 0)
  (0 0 2 0))

   ((1 3 4 2)
  (4 2 3 1)
  (2 4 1 3)
  (3 1 2 4)));14


  (((0 0 0 0)
  (4 3 0 2)
  (2 0 0 4)
  (0 0 0 0))

   ((1 2 4 3)
  (4 3 1 2)
  (2 1 3 4)
  (3 4 2 1)));15

  (((0 1 0 0)
  (0 0 2 0)
  (0 0 0 3)
  (4 0 0 0))

   ((2 1 3 4)
  (3 4 2 1)
  (1 2 4 3)
  (4 3 1 2)));16

  (((0 2 0 1)
  (0 0 2 0)
  (4 1 0 0)
  (0 0 0 0))

   ((3 2 4 1)
  (1 4 2 3)
  (4 1 3 2)
  (2 3 1 4)));17

     (((0 0 0 3)
  (2 0 0 0)
  (0 4 0 0)
  (0 0 1 0))

   ((4 1 2 3)
  (2 3 4 1)
  (1 4 3 2)
  (3 2 1 4)));18

   (((0 0 2 0)
  (1 0 0 0)
  (0 4 0 0)
  (0 0 0 3))

   ((4 3 2 1)
  (1 2 3 4)
  (3 4 1 2)
  (2 1 4 3)));19

  (((1 4 0 0)
  (0 0 0 0)
  (0 0 0 0)
  (0 0 2 4))

   ((1 4 3 2)
  (2 3 4 1)
  (4 2 1 3)
  (3 1 2 4)));20
   );closing parenthesis
  )

(define (test Sboards)
  (if (empty? Sboards)
      #t
      (and (equal? (solve (car (car Sboards))) (car (cdr (car Sboards)))) (test (cdr Sboards)))
      )
  )


(test solvedboards)


;These are 20 cases that show that the algorithm works correctly. I used the url
;http://www.sudoku-download.net/files/60_Sudokus_4x4_Difficult.pdf to get boards that are solvable
;http://www.sudoku-download.net/files/Solution_60_Sudokus_4x4_Difficult.pdf to get the solutions of the 4x4 sudoku