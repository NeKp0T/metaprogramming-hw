#lang racket

(require syntax/datum racket/undefined)

(struct flowState (varMap blockMap currentLabel) #:transparent #:mutable)

(define operators '(car cdr cons + - * /))

(define (flowParse program values)
    (define varList (cdar program))
    (define varMap (map cons varList values))
    (define blockMap (cdr program))
    (flowState varMap blockMap (caar blockMap)))

(define (evaluateOperator state op arg1 arg2)
    (display " op, args:")
    (display op)
    (display arg1)
    (define a1 (flowEval state arg1))
    (define a2 (flowEval state arg2))
    (eval `(,op (quote ,a1) (quote ,a2))))

(define (flowEval state expr)
    (match expr
        [`(quote ,value) value]
        [`(,op ,arg1 ,arg2) (evaluateOperator state op arg1 arg2)]
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