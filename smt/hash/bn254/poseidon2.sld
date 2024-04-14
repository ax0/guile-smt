;; Poseidon2-on-BN254 hasher.
;; Calls out to Rust library.
(define-library (smt hash bn254 poseidon2)
  (export poseidon2-bn254-leaf-hash
	  poseidon2-bn254-branch-hash)
  (import
   (guile)
   (scheme base)
   (smt instance bn254)
   (system foreign)
   (system foreign-library))
  (begin
    ;; libguile_smt_hash.so should be in $LD_LIBRARY_PATH
    (define libguile-smt-hash (dynamic-link "libguile_smt_hash"))

    (define (ff c-name args return-type)
      (pointer->procedure return-type (dynamic-func c-name libguile-smt-hash) args))

    ;; Helper functions for brevity
    (define number->string-ptr (lambda (x) (string->pointer (number->string x))))
    (define bvtoi (lambda (bv len) (string->number (utf8->string bv 0 len))))

    ;; Leaf hasher
    (define poseidon2-bn254-leaf-hash
      (let ((%hash-leaf
	     (ff "c_hash_leaf" '(* * *) size_t)))
	(lambda (key value)
	  (define hash-string-bytes (make-bytevector bn254-max-digits 0))
	  (define hash-string-byte-length
	    (%hash-leaf (number->string-ptr key)
			(number->string-ptr value)
			(bytevector->pointer hash-string-bytes)))
	  (bvtoi hash-string-bytes hash-string-byte-length))))

    ;; Branch hasher
    (define poseidon2-bn254-branch-hash
      (let ((%hash-branch
	     (ff "c_hash_branch" '(* * *) size_t)))
	(lambda (l r)
	  (define hash-string-bytes (make-bytevector bn254-max-digits 0))
	  (define hash-string-byte-length
	    (%hash-branch (number->string-ptr l)
			  (number->string-ptr r)
			  (bytevector->pointer hash-string-bytes)))
	  (bvtoi hash-string-bytes hash-string-byte-length))))))
