(define-library (smt proof)
  (export smt-proof
	  smt-proof?
	  smt-inclusion-proof?
	  smt-proof-key
	  smt-proof-value
	  smt-proof-siblings

	  smt-get-adjacent-proof
	  smt-get-proof)
  (import (scheme base)
	  (smt)
	  (smt types))
  (begin
    ;; Record type for SMT proofs
    (define-record-type <smt-proof>
      (smt-proof inclusion-indicator key maybe-value siblings)
      smt-proof?
      (inclusion-indicator smt-inclusion-proof?)
      (key smt-proof-key)
      (maybe-value smt-proof-value)
      (siblings smt-proof-siblings))

    ;; Proof getter. Outputs a boolean indicating whether the proof is one of inclusion,
    ;; the key of the proof, maybe the value and the siblings in ascending order of depth.
    (define (smt-get-proof smt key)
      (define key-bits ((smt-key->bits smt) key))
      (define-values (key-path siblings --leaf-depth)
	(smt-trace smt key-bits)) ; Trace path

      ;; Determine terminal node
      (define terminal-node-hash (car key-path))
      (define terminal-node (maybe-map (lambda (h) (smt-ref smt h)) terminal-node-hash))
      (define terminal-key-bits (and (leaf? terminal-node) ((smt-key->bits smt) (leaf-key terminal-node))))

      (define inclusion-indicator (equal? terminal-key-bits key-bits))
      (define-values (terminal-key terminal-value)
	(if (leaf? terminal-node) (values (leaf-key terminal-node) (leaf-value terminal-node))
	    (values key #f)))
      (define proof-siblings (reverse siblings))

      (smt-proof inclusion-indicator terminal-key terminal-value proof-siblings))

    ;; One way of processing the deletion of a key is to use the proof for another key having this key as
    ;; a sibling. This getter does just that unless there is no adjacent proof (i.e. the case of a singleton
    ;; tree).
    ;; TODO: Optimise.
    (define (smt-get-adjacent-proof smt key)
      (define key-bits ((smt-key->bits smt) key))
      (define-values (--key-path proof-siblings --leaf-depth) (smt-trace smt key-bits))
      (if (null? proof-siblings)
	  (smt-get-proof smt key)
	  (let loop ((sibling (car proof-siblings)));; Never #f due to SMT optimisation
	    (let ((sibling-node (smt-ref smt sibling)))
	      (if (leaf? sibling-node)
		  (smt-get-proof smt (leaf-key sibling-node))
		  (or (maybe-map loop (branch0 sibling-node))
		      (maybe-map loop (branch1 sibling-node))))))))))
