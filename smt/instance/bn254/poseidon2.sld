(define-library (smt instance bn254 poseidon2)
  (export make-bn254-poseidon2-smt)
  (import (scheme base)
	  (smt hash bn254 poseidon2)
	  (smt instance)
	  (only (smt instance bn254) bn254-number-max))
  (begin
    ;; SMT with Poseidon 2 hash function on BN254.
    (define (make-bn254-poseidon2-smt smt-db max-depth)
      (make-number-smt smt-db poseidon2-bn254-leaf-hash poseidon2-bn254-branch-hash 0 bn254-number-max max-depth))))
