#lang racket

(require syntax/datum)
(provide tm-int-step tm-int-steps tm-int)

;;; (code, lineNumber, (leftList, rightList))

(define (carOrDef lst)
    (if (null? lst) -1 (car lst)))

(define (tailOrDef lst)
    (if (null? lst) '() (rest lst)))

(define setHead
    (lambda (val lst)
        (cons val (tailOrDef lst))))

(define moveLeft
    (lambda (leftList rightList)
        (list (tailOrDef leftList) (cons (carOrDef leftList) rightList))))

(define moveRight
    (lambda (leftList rightList)
        (reverse (moveLeft rightList leftList))))

(define tm-int-step
    (lambda (code lineNumber leftList rightList)
        (if (>= lineNumber (length code))
            (error "execution finished")
            (match (list-ref code lineNumber)
                ['(left) 
                    (append 
                        `(,code ,(+ 1 lineNumber))
                        (moveLeft leftList rightList))]
                ['(right) 
                    (append 
                        `(,code ,(+ 1 lineNumber))
                        (moveRight leftList rightList))]
                [`(write ,c) (list code (+ 1 lineNumber) (setHead c leftList) rightList)]
                [`(goto ,i) (list code i leftList rightList)]
                [`(if ,c goto ,i) 
                    (if (equal? c (carOrDef leftList))
                        (list code i leftList rightList)
                        (list code (+ 1 lineNumber) leftList rightList))]
                ))))

(define (tm-int-steps code lineNumber leftList rightList)
    (if (>= lineNumber (length code))
        `(,leftList ,rightList)
        (match (tm-int-step code lineNumber leftList rightList)
            [`(,code1 ,lineNumber1 ,leftList1 ,rightList1) (tm-int-steps code1 lineNumber1 leftList1 rightList1)] ; how to do this without pattern-matching?
        )))
        ;;(eval  (cons tm-int-steps (map quote (tm-int-step code lineNumber leftList rightList))))))

(define (tm-int code leftList rightList) (tm-int-steps code 0 leftList rightList))

(define test
    '((left left) 0 () (0 1)))

(define tm-example '((if 0 goto 3) (right) (goto 0) (write 1)))

(define tm-example2 '((if -1 goto 3) (right) (goto 0) (write 1)))

(define tm-result (tm-int tm-example '(1) '(1 1 1 0 1 1 1)))

(define tm-result2 (tm-int tm-example2 '(1) '(1 1 1 0 1 1 1)))