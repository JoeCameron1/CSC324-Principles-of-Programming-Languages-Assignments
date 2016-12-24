#lang racket

#|
This constructor macro is based on the original class macro. This class macro with constructor adds an
extra field constructorMacro, needed for the syntax-rules.

The <functionName> is the name of the constructor function.
The <constructor> is the constructor function that will define the variables
of the class.

In order to instantiate the class, define the name of the constructor,
followed by the __init__ function, this class (MyClass), and a list of the values a and b.

Here is an example: (define Name_Of_Class_Instance_Here (__init__ MyClass (list a b))), where a and b are integers.
Note that __init__ contains two underscores on either side of init, not one.
|#

(define-syntax class
  (syntax-rules (constructorMacro)
    [(class <Class> (<attr> ...)
       [(constructorMacro <functionName> <constructor>)]
       [(<method> <param> ...) <body>] ...)
     (begin
       (define (<Class> <attr> ...)
         (lambda (msg)
           (cond
             [(equal? msg (id->string <attr>)) <attr>]
             ...
             [(equal? msg (id->string <method>))
              (lambda (<param> ...) <body>)]
             ...
             [else "Unrecognized message!"])))
       ;Using the construcutor function to instantiate
       (define (<functionName> <Class> [<constrargs> `()])
         ((apply <constructor> <constrargs>) <Class>))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))


#|###################################################################
---------------------------------------------------------------------

Translating this python code:

class MyClass:
    def __init__ (self , a, b):
        r = f(a)
        self.x = f(a)
        self.y = [b, 100, r]
        self.z = ”you are cool”

def f(r):
  return r + 5

into racket.

---------------------------------------------------------------------
|#

(define (f r)
  (+ r 5))

;Defining the constructor function
(define (Constructor a b)
  (let* ([r (f a)]
         [x (f a)]
         [y (list b 100 r)]
         [z "you are cool"])
    (lambda (self) (self x y z)))) ;This last line must be present

;Creating MyClass
(class MyClass (x y z)
  [(constructorMacro __init__ Constructor)])