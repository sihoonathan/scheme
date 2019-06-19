(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
 (map (lambda (x) (append (list first) x)) rests)
)

(define (zip pairs)
 (define left (map (lambda (x) (car x)) pairs))
 (define right (map (lambda (x) (car (cdr x))) pairs))
 (list left right)
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
 (define (helper index count s)
  (cond ((= count 0) '())
   (else (cons (list index (car s)) (helper (+ 1 index) (- count 1) (cdr s)))
   )
  )
 )
  (helper 0 (length s) s)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
 (cond ((= total 0) '(()))
  ((or (< total 0) (= (length denoms) 0)) '())
  (else
   (define using_largest_denoms (list-change (- total (car denoms)) denoms))
   (define with_largest_denom (cons-all (car denoms) using_largest_denoms))
   (define without_largest_denom (list-change total (cdr denoms)))
   (append with_largest_denom without_largest_denom)
  ))
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         'replace-this-line
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         'replace-this-line
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           'replace-this-line
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           'replace-this-line
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         'replace-this-line
         ; END PROBLEM 19
         )))
