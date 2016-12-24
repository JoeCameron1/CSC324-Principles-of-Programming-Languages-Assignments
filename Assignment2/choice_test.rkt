#lang plai
(require "choice_uses.rkt")
(require "choice.rkt")

; Helper for comparing lists of subsets
(define (compare-multi lst1 lst2)
  (and (equal? (length lst1) (length lst2))
       (equal? (list->set (map (lambda (x) (sort x <)) lst1))
               (list->set (map (lambda (x) (sort x <)) lst2)))))


; Tests for subsets
(test (all (subsets '()))
      '(()))

(test (compare-multi (all (subsets '(1)))
                     '(() (1)))
      #t)

(test (compare-multi (all (subsets '(1 2 3)))
                     '(()
                       (1)
                       (2)
                       (2 1)
                       (3)
                       (1 3)
                       (2 3)
                       (1 2 3)))
      #t)

; Test for sudoku-4
(define grid1
  '((1 2 3 4)
    ("" "" 1 "")
    ("" "" 2 3)
    (2 "" "" 1)))

(test (sudoku-4 grid1)
      '((1 2 3 4)
        (3 4 1 2)
        (4 1 2 3)
        (2 3 4 1)))

; Clear stack
(clear)

; Test for fold-<
(test (fold-< max 0 (sin (* (-< 1 2 3 4) (+ (-< 100 200) (-< 1 2)))))
      0.9948267913584064)
      