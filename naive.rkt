#lang racket

;; ('proof 'a)
;; ('infer 'a 'b)
;; ('fail)
;; ('both 'a 'b)
;; ('either 'a 'b)

(define (do-proof statements)
  (let ((proofs (filter (lambda (x) (eq? 'proof (car x))) statements))
        (infers (filter (lambda (x) (eq? 'infer (car x))) statements)))
    (apply-infer proofs infers)))

(define (sat proofs p)
  (if (null? proofs)
      #f
      (if (eq? (cadr (car proofs)) p)
          #t
          (sat (cdr proofs) p))))

(define (apply-infer proofs infers)
  (define (handle-infer inf)
    (if (pair? (cadr inf))
      (cond ((eq? 'both (car (cadr inf)))
             (and (sat proofs (cadr (cadr inf))) (sat proofs (caddr (cadr inf)))))
            ((eq? 'either (car (cadr inf)))
             (or (sat proofs (cadr (cadr inf))) (sat proofs (caddr (cadr inf)))))
            (else 'fail))
      (sat proofs (cadr inf))))
  (let ((inf (car infers)))
    (if (null? (cdr infers))
        (if (handle-infer inf)
            (caddr inf)
            'fail )
        (if (handle-infer inf)
            (apply-infer (cons (list 'proof (caddr inf)) proofs) (cdr infers))
            'fail ))))

; a
; a -> b
; (a ^ b) -> c
; ------------
; c
(do-proof '((proof a) (infer a b) (infer (both a b) c)))
