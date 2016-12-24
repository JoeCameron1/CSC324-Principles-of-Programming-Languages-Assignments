#| Choice Implementation with Peek

This is a cleaned-up version of the code from lecture.
While you are responsible for both understanding the
implementation and public API, only the latter is
required for this assignment.

We strongly recommend not changing this file.
########################################################################
NEW

This is peek.rkt, it's an alteration of the choices implementation.

We are implementing the peek functionality, without affecting/consuming choices.
To achieve the new peek functionality, we have made 3 new functions: peeks, pushQuote! and peek.

'peeks' represents the quoted choice stack (similar to choices except the choices are quoted/used by peek).

'pushQuote!' adds quoted choices to the peeks stack (similar to add-choices! except the choices are quoted/used by peek).

'peek' views the top of the peeks stack.

#########################################################################

|#
#lang racket
(provide -< next all clear ?- peek)

#|
(-< <expr> ...)
  Each <expr> is an arbitrary Racket expression.

  Evaluates and returns the first <expr>.
  If there is more than one argument, stores a choice point
  which resumes the program at where the (-< ...) is used,
  but with the remaining choices.
|#
(define-syntax -<
  (syntax-rules ()
    ; When there is only one option, return it.
    [(-< <expr1>) <expr1>]
    ; When there is more than one, return the first and store the rest.
    [(-< <expr1> <expr2> ...)
     (let/cc cont
       ; Push a new choice onto choices.
       (add-choice! (lambda () (cont (-< <expr2> ...))))
       ;#################################################
       ;NEW
       ; Push a quoted version of choice onto our peeks stack.
       (pushQuote! (lambda () '(-< <expr2> ...)))
       ;#################################################
       <expr1>)]))


#|
(next)

  Backtracks to the most recently stored choice point and
  resume program execution from there, or returns "false."
  if there are no choice points stored.
> (-< 1 2 3)
1
> (next)
2
> (next)
3
> (next)
"false."
|#
(define (next)
  ; Check if there are any remaining choices
  (if (empty? choices)
      "false."
      ; Notice that it's ((get-choice!)) and not (get-choice!).
      ; What's the difference?
      ((get-choice!))))


#|
(all <expr>)
  <expr> is a Racket expression, possibly containing choice points.
  
  Returns a list of all possible outcomes of evaluating <expr>
  (i.e., making all possible combinations of the choices).
|#
(define-syntax all
  (syntax-rules ()
    [(all <expr>)
     (let* (
            ; The list to store all the choices. Because macros are hygienic,
            ; we don't get name conflicts if we use all multiple times.
            [all-results '()]
            ; A helper function which mutates all-results.
            [all-helper 
             (lambda (e)
               (set! all-results (cons e all-results))
               ; If (next) calls a continuation, the last expression
               ; (reverse choices) will not execute (and hence not be
               ; returned. But if (next) returns "false." then the last
               ; expression does get executed.
               (next)
               (reverse all-results))])
       (all-helper <expr>))]))

#|
(clear)

  Remove all choice points from stack. Used for testing purposes.
|#
(define (clear)
  (set! choices '()))


#|
(?- pred expr)
  pred: a unary predicate (i.e., boolean function)
  expr: a choice expression

  Returns a choice from 'expr' which satisfies 'pred'.
  Calling 'next' will return all possible choices
  that satisfy 'pred'.


|#
(define (?- pred expr)
  (if (pred expr)
      expr
      ; If the predicate fails, try the next choice.
      (next)))


;------------------------------------------------------------------------------
; Private values for managing the stack of choices.
;------------------------------------------------------------------------------

; The stack of choice points, represented as a list.
(define choices '())

; "Push": add a choice to the choices stack.
(define (add-choice! choice)
  (set! choices
        (cons choice choices)))

;###############################################################################
;NEW ADDITIONS added by us to the original choice.rkt
; to satify requirements of peek.rkt

; The stack of new choice points (used for peek functionality),
; represented as a list.
(define peeks '())

;"PushQuote": add a quoted choice to the peek stack.
(define (pushQuote! choice)
  (set! peeks (cons choice peeks)))

;"Peek": View the top of the stack without changing things.
(define (peek)
  (if (empty? peeks) ;return "false" if peeks is empty (same as when choices is empty) otherwise return the quoted choice peek has stored in its stack
      "false."
      ((first peeks))))

;###############################################################################

; "Pop": remove and return first choice from
; the choices stack.
(define (get-choice!)
  (let ([choice (first choices)])
    (set! choices (rest choices))
    (set! peeks (rest peeks)) ;NEW Peek function: Referring to peeks instead of choices now.
    choice))