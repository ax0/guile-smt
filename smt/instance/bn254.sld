(define-library (smt instance bn254)
  (export bn254-number-max bn254-max-digits)
  (import (scheme base))
  (begin
    ;; BN254 scalar field size
    (define bn254-number-max 21888242871839275222246405745257275088548364400416034343698204186575808495617)

    ;; Maximum number of decimal digits in a BN254 scalar field element
    (define bn254-max-digits 77)))
