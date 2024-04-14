(define-library (smt aux)
  (export number->le-bits
	  first-unequal-index-from)
  (import
   (scheme base))
  (begin
(define (first-unequal-index-from i vec1 vec2)
  (let loop ((i i))
    (cond ((or (>= i (vector-length vec1)) (>= i (vector-length vec2))) #f)
	  ((not (eq? (vector-ref vec1 i) (vector-ref vec2 i))) i)
	  (else (loop (+ i 1))))))))
