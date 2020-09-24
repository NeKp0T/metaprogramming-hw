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
    ;;; (display " op, args:")
    ;;; (display op)
    ;;; (display args)
    ;;; (display " | ")
    (define argsEvaled (map (lambda (a) (flowEval state a)) args))
    ;;; (define toev `,(list* op ae))
    ;;; (display " toev:")
    ;;; (display (eval op))
    (apply (eval op (make-base-namespace)) argsEvaled))

(define (fs a b) a)
(define (sd a b) b)

(define (flowEval state expr)
    (match expr
        [`(quote ,value) value]
        [(and (list* op args) (list* _ _ _)) (evaluateOperator state op args)]
        [var (cdr (assoc var (flowState-varMap state)))]))

(define (changeVar list var val)
    (map (lambda (key . oldval)
            (if (eq? key var)
                (cons key val)
                (cons key oldval)))
        list))

(define (flowStep state)
    (define block (cdr (assoc (flowState-currentLabel state) (flowState-blockMap state))))
    ;;; (display (flowState-currentLabel state))
    (for ([aj block])
        (match aj
            [`(:= ,var ,expr)
                (set-flowState-varMap! state (changeVar (flowState-varMap state) var (flowEval state expr)))
                state]
            [`(goto ,label)
                (set-flowState-currentLabel! state label)
                state]
            [`(if ,expr ,ifTrue ,ifFalse)
                (set-flowState-currentLabel! state (if (flowEval state expr) ifTrue ifFalse))
                state]
            [`(return ,expr)
                (flowEval state expr)]))
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


(define testState (flowParse find_name '(x (a x c) (z t e))))

(define test1
    (flowStep testState))


