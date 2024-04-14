(define-library (smt types)
  (export branch
	  branch?
	  branch0
	  branch1
	  branch-ref
	  branch-with-parity ;;
	  leaf
	  leaf?
	  leaf-key
	  leaf-value

	  number-node->string
	  string->number-node

	  maybe->t
	  maybe-map
	  maybe-number->string
	  maybe-modulo)
  (import (scheme base)
	  (srfi 13) #| strings |#)
  (begin
    ;; For simplicity, we represent branches in the form
    ;; (branch hash0 hash1)
    ;; and leaves in the form
    ;; (leaf key value).

    (define-record-type <branch>
      (branch hash0 hash1)
      --branch?
      (hash0 branch0)
      (hash1 branch1))

    ;; Branch should never have both hashes be false
    (define (branch? node)
      (and (--branch? node) (or (branch0 node) (branch1 node)) #t))

    (define (branch-ref i branch)
      (if (zero? i) (branch0 branch)
	  (branch1 branch)))

    (define (branch-with-parity bit b0 b1)
      (apply branch (if (zero? bit)
			(list b0 b1)
			(list b1 b0))))

    (define-record-type <leaf>
      (leaf key value)
      leaf?
      (key leaf-key)
      (value leaf-value))

    (define (invalid-node-type-error)
      (error "Invalid node"))


    ;; For convenience, we provide conversions for number and bytevector types.
    (define (number-node->string number-max node)
      (cond ((branch? node)
	     (string-join `("branch" ,(maybe-number->string (branch0 node))
			    ,(maybe-number->string (branch1 node)))))
	    ((leaf? node)
	     (string-join `("leaf" ,(number->string (maybe-modulo (leaf-key node) number-max))
			    ,(number->string (maybe-modulo (leaf-value node) number-max)))))
	    (else (invalid-node-type-error))))

    (define (string->number-node str)
      (define (string->maybe-number str)
	(if (eq? str "#f") #f (string->number str)))
      (define tokenised-str (string-tokenize str))
      (case (string->symbol (car tokenised-str))
	((branch) (apply branch (map string->maybe-number (cdr tokenised-str))))
	((leaf) (apply leaf (map string->number (cdr tokenised-str))))
	(else => (invalid-node-type-error)))
      )

    ;; Convenience procedures for optional ('maybe') values
    (define (maybe->t m default)
      (if m m default))

    (define (maybe-map f m)
      (if m (f m) #f))

    (define (maybe-number->string m)
      (maybe->t (maybe-map number->string m) "#f"))

    (define (maybe-modulo n m)
      (if m (modulo n m) n))))
