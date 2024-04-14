(define-library (smt instance)
  (export make-number-smt
	  number->le-bits
	  smt-overlay)
  (import (scheme base)
	  (smt)
	  (smt db)
	  (smt types)
	  (srfi 69))
  (begin
    ;; Auxiliary procedures for SMT operations
    (define number->le-bits (lambda (num-bits)
			      (lambda (key)
				(define bits (make-vector num-bits 0))
				(let bit-loop ((i 0) (k key))
				  (cond ((zero? k) bits)
					((= i num-bits) bits)
					(else (begin
						(vector-set! bits i (modulo k 2))
						(bit-loop (+ i 1) (quotient k 2)))))))))

    ;; Number-based SMT with flexible backend.
    (define (make-number-smt smt-db leaf-hash branch-hash default-value number-max max-depth)
      (define key->bits (if number-max (lambda (k) ((number->le-bits max-depth) (modulo k number-max))) (number->le-bits max-depth)))
      (make-smt smt-db max-depth default-value leaf-hash branch-hash key->bits))

    ;; Overlay SMT
    (define (smt-overlay smt)
      (define (make-smt-overlay-db db)
	(define added #f) ; Table of added/updated nodes (Hash -> Node)
	(define deleted #f) ; Table of deleted nodes (Hash -> Bool)
	(define root #f) ; Root of overlay
	(define open (lambda () (begin
			     (set! added (make-hash-table))
			     (set! deleted (make-hash-table)))))
	(define close (lambda () (begin
			      (set! added #f)
			      (set! deleted #f)
			      (set! root #f))))
	(define get-root (lambda () (if root root ((smt-db-get-root db)))))
	(define set-root! (lambda (r) (set! root r)))
	(define put! (lambda (hash node) (hash-table-set! added hash node)))
	(define ref (lambda (hash) (let ((deleted-node (hash-table-ref/default deleted hash #f))
				    (overlaid-node (hash-table-ref/default added hash #f)))
				(cond (deleted-node #f)
				      (overlaid-node overlaid-node)
				      (else ((smt-db-ref db) hash))))))
	(define delete! (lambda (hash)
			  (begin
			    (hash-table-delete! added hash)
			    (hash-table-set! deleted hash #t))))
	(make-smt-db open close get-root set-root! put! ref delete!))
      (make-smt (make-smt-overlay-db (smt-db smt)) (smt-max-depth smt) (smt-default-value smt)
		(smt-leaf-hash smt) (smt-branch-hash smt) (smt-key->bits smt)))))
