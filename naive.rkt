#lang racket

;; ('proof 'a)
;; ('infer 'a 'b)
;; ('fail)
;; ('and 'a 'b)
;; ('or 'a 'b)
;; ('not 'a)

(define (simplify pf)

  (define (handle-and np p)
    (let ((tag (car p)) (body (cdr p)))
      (cond ((eq? tag 'or ) 
            (let ((l (car body)) (r (cadr body)))
              (cond ((eq? np l) (simplify np))
                    ((eq? np r) (simplify np))
                    (else (append (list 'proof np) (simplify p))))))
            (else (append (list 'proof np) (simplify p))))))

  (define (handle-or np p)
    (let ((tag (car p)) (body (cdr p)))
      (cond ((eq? tag 'and ) 
            (let ((l (car body)) (r (cadr body)))
              (cond ((eq? np l) (simplify np))
                    ((eq? np r) (simplify np))
                    (else (append (list 'proof np) (simplify p))))))
            (else (append (list 'proof np) (simplify p))))))
  
  (if (not (pair? pf))
    pf
    (let ((tag (car pf)) (body (cdr pf)))
      (cond ((eq? tag 'not )
             (if (pair? (car body))
                (let ((tag-b (car (car body))) (body-b (cdr (car body))))
                  (cond ((eq? 'not tag-b) 
                        (simplify (car body-b)))
                       ;; DeMorgan's Law
                       ((eq? 'and tag-b)
                        (list 'or (simplify (list 'not (car body-b))) (simplify (list 'not (cadr body-b)))))
                       ((eq? 'or tag-b)
                        (list 'and (simplify (list 'not (car body-b))) (simplify (list 'not (cadr body-b)))))
                       (else (list 'not body))))
                 pf))
            ((eq? tag 'and )
              (let ((left (car body)) (right (cadr body)))
                (cond ((eq? left right) left)
                      ((and (pair? left) (not (pair? right))) (handle-and right left))
                      ((and (pair? right) (not (pair? left))) (handle-and left right))
                      (else (list 'and (simplify (car body)) (simplify (cadr body)))))))
            ((eq? tag 'or )
              (let ((left (car body)) (right (cadr body)))
                (cond ((eq? left right) left)
                      ((and (pair? left) (not (pair? right))) (handle-or right left))
                      ((and (pair? right) (not (pair? left))) (handle-or left right))
                      (else (list 'or (simplify (car body)) (simplify (cadr body)))))))
            (else pf)))))


(define (expend pf)
  (let ((tag (car pf)) (body (cdr pf)))
    (if (eq? tag 'proof ) (expend-pf (car body))
           (expend-pf pf))))

(define (expend-pf pf)
  (if (not (pair? pf))
    (list (list 'proof pf))
    (let ((tag (car pf)) (body (cdr pf)))
      (cond ((eq? tag 'not ) (list (list 'proof pf)))
            ((eq? tag 'and ) (append (expend-pf (car body)) (expend-pf (cadr body))))
            (else (list (list 'proof pf)))))))

(define (simp-all pf)
  (let ((tag (car pf)) (body (cdr pf)))
    (cond ((eq? tag 'proof )
           (simplify (car body)))
          ((eq? tag 'infer )
           (list 'infer (simplify (car body)) (simplify (cadr body)))))))

(define (do-proof statements)
  (let ((proofs (foldr append '() (map expend (filter (lambda (x) (eq? 'proof (car x))) (map simp-all statements)))))
        (infers (filter (lambda (x) (eq? 'infer (car x))) (map simp-all statements))))
    (begin
      (displayln proofs)
      (displayln infers)
      (displayln (apply-infer proofs infers)))))


(define (sat proofs p)
  (if (null? proofs)
      #f
      (or (eq? (cadr (car proofs)) p)
          (sat (cdr proofs) p))))

(define (apply-infer proofs infers)
  (define (handle-infer inf)
    (if (pair? (cadr inf))
      (cond ((eq? 'and (car (cadr inf)))
             (and (sat proofs (cadr (cadr inf))) (sat proofs (caddr (cadr inf)))))
            ((eq? 'or (car (cadr inf)))
             (or (sat proofs (cadr (cadr inf))) (sat proofs (caddr (cadr inf)))))
            (else 'fail ))
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
(do-proof '(
  (proof (and (not p) q))
  (infer q r)
  (infer r t)
))

          
