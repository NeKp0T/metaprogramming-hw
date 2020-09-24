#lang racket

(require syntax/datum racket/undefined)

(struct flowState (varMap blockMap currentLabel) #:transparent #:mutable)

(define operators '(car cdr cons + - * /))

(define (flowParse program values)
    (define varList (cdar program))
    (define varMap (map cons varList values))
    (define blockMap (cdr program))
    (flowState varMap blockMap (caar blockMap)))

(define (evaluateOperator state op args)
    (display " op, args:")
    (display op)
    (display args)
    (display " | ")
    (define ae (map (lambda (a) (flowEval state a)) args))
    (define toev `,(list* op ae))
    (display " toev:")
    (display (eval op))
    (apply (eval op) ae))

(define (fs a b) a)
(define (sd a b) b)

(define (flowEval state expr)
    (match expr
        [`(quote ,value) value]
        [(and (list* op args) (list* _ _ _)) (evaluateOperator state op args)]
        [var (display var) (cdr (assoc var (flowState-varMap state)))]))

(define (flowStep state)
    (define block (cdr (assoc (flowState-currentLabel state) (flowState-blockMap state))))
    (for ([aj block])
        (match aj
            [`(:= ,var ,expr)
                undefined]
            [`(goto ,label)
                undefined]
            [`(if ,expr ,ifTrue ,ifFalse)
                undefined]
            [`(return ,expr)
                undefined]))
    )



(define find_name
    '((read name namelist valuelist)
     (search (if (equal? name (car namelist)) found cont))
     (cont (:= valuelist (cdr valuelist))
           (:= namelist (cdr namelist))
           (goto search))
     (found (return (car valuelist)))
     (trash (:= valuelist (quote (1 2 3))))
    ))

(define test1
    (flowStep (flowParse find_name '(x y z))))
(define testState (flowParse find_name '(x y z)))