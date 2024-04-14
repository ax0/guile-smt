(define-library (smt)
  (export make-smt
	  smt?
	  smt-db
	  smt-max-depth
	  smt-default-value
	  smt-leaf-hash
	  smt-branch-hash
	  smt-key->bits

	  smt-open
	  smt-close
	  smt-key-ref
	  smt-ref
	  smt-root
	  smt-trace
	  smt-insert!
	  smt-update!
	  smt-delete!)
  (import (scheme base)
	  (smt aux)
	  (smt db)
	  (smt types)
	  (srfi 1))
  (begin
    ;; An SMT consists of:
    ;; - an <smt-db> storing the root and nodes,
    ;; - a maximum depth,
    ;; - a choice of default/empty value,
    ;; - a two-parameter hash function for leaves (i.e. key-value pairs),
    ;; - a two-parameter hash function for branches (i.e. hash pairs) and
    ;; - a function for converting keys to bits.
    ;; It is assumed that `equal?` is defined on the hash and key types.
    (define-record-type <sparse-merkle-tree>
      (make-smt db max-depth default-value leaf-hash branch-hash key->bits)
      smt?
      (db smt-db)
      (max-depth smt-max-depth)
      (default-value smt-default-value)
      (leaf-hash smt-leaf-hash)
      (branch-hash --smt-branch-hash)
      (key->bits smt-key->bits))

    ;; Allow branch hash function to apply to #f
    (define (smt-branch-hash smt)
      (lambda (b0 b1)
	(apply (--smt-branch-hash smt) (map (lambda (hash) (maybe->t hash (smt-default-value smt))) (list b0 b1)))
	))


    ;; Convenience procedures
    (define (smt-open smt)
      ((smt-db-open (smt-db smt))))
    
    (define (smt-close smt)
      ((smt-db-close (smt-db smt))))

    (define (smt-set! smt hash node)
      ((smt-db-set! (smt-db smt))
       hash node))

    (define (smt-ref smt hash)
      ((smt-db-ref (smt-db smt))
       hash))

    (define (smt-root smt)
      ((smt-db-get-root (smt-db smt))))

    (define (smt-set-root! smt root)
      ((smt-db-set-root! (smt-db smt)) root))

    (define (smt-delete-node! smt hash)
      (maybe-map (lambda (hash) ((smt-db-delete! (smt-db smt))
			    hash)) hash))

    ;; Trace the path described by the bit array `key-bits` until a leaf
    ;; is (maybe) reached. Output the path (including root), the siblings
    ;; encountered on the way and the depth of the leaf reached.
    (define (smt-trace smt key-bits)
      (let loop ((current-node-hash (smt-root smt))
		 (path '())
		 (siblings '())
		 (leaf-depth 0))
	(let ((current-node (smt-ref smt current-node-hash)))
	  (cond ((branch? current-node)
		 (let ((current-bit (vector-ref key-bits leaf-depth)))
		   (loop (branch-ref current-bit current-node)
			 (cons current-node-hash path)
			 (cons (branch-ref (- 1 current-bit) current-node) siblings)
			 (+ 1 leaf-depth))))
		(else
		 (values
		  (cons current-node-hash path)
		  siblings
		  leaf-depth))))))

    (define (smt-key-ref smt key)
      (define key-bits ((smt-key->bits smt) key))
      (define-values (path --siblings --leaf-depth) (smt-trace smt key-bits))
      (define terminal-node (smt-ref smt (car path)))
      (and (leaf? terminal-node) (equal? key-bits ((smt-key->bits smt) (leaf-key terminal-node))) (leaf-value terminal-node)))

    ;; Compute path leading to leaf. Outputs an alist of (hash, node) pairs in ascending order.
    (define (smt-compute-path smt key-bits index leaf siblings)
      (define leaf-hash (maybe-map (lambda (leaf) ((smt-leaf-hash smt) (leaf-key leaf) (leaf-value leaf))) leaf))
      (car (fold (lambda (sibling acc)
		   (let* ((previous-hash (caaar acc))
			  (i (cdr acc))
			  (bit (vector-ref key-bits i))
			  (new-branch (branch-with-parity bit previous-hash sibling))
			  (new-branch-hash (apply (smt-branch-hash smt) (map (lambda (m) (maybe->t m (smt-default-value smt))) (list (branch0 new-branch) (branch1 new-branch))))))
		     (cons (cons (cons new-branch-hash new-branch) (car acc)) (- i 1))))
		 (cons (list (cons leaf-hash leaf)) index)
		 siblings)))

    ;; Insert a key-value pair in the SMT, updating the root as well as inserting and deleting nodes as
    ;; appropriate.
    (define (smt-insert! smt key value)
      ;; Trace down
      (define key-bits ((smt-key->bits smt) key))
      (define-values (key-path siblings leaf-depth) (smt-trace smt key-bits))

      ;; Determine terminal node
      (define terminal-node-hash (car key-path))
      (define terminal-node (maybe-map (lambda (h) (smt-ref smt h)) terminal-node-hash))
      (define terminal-key-bits (and (leaf? terminal-node) ((smt-key->bits smt) (leaf-key terminal-node))))

      ;;  If the terminal node is a (non-empty) leaf, its key-to-bit decoposition must be different.
      (if (equal? key-bits terminal-key-bits)
	  #f
	  (let-values (((new-siblings new-branch-depth)
			(cond
			 ((leaf? terminal-node)
			  (let* ((first-unequal-index (first-unequal-index-from leaf-depth key-bits terminal-key-bits))
				 (num-fillers (maybe-map (lambda (i) (- i leaf-depth)) first-unequal-index))
				 (terminal-siblings (if num-fillers (cons terminal-node-hash (make-list num-fillers #f)) '())))
			    (values (append terminal-siblings siblings) (maybe->t first-unequal-index (- leaf-depth 1)))))
			 (else
			  (values siblings (- leaf-depth 1))))))
	    (let* ((new-leaf (leaf key value)) ; Compute new leaf
		   (new-path (smt-compute-path smt key-bits new-branch-depth new-leaf new-siblings))) ; Compute new path
	      
	      ;; Commit new nodes to DB
	      (for-each (lambda (pair) (smt-set! smt (car pair) (cdr pair))) new-path)
	      
	      ;; Set new root
	      (smt-set-root! smt (caar new-path))
	      
	      ;; Delete nodes along former path other than the leaf (if any)
	      (for-each (lambda (hash) (smt-delete-node! smt hash)) (cdr key-path)))
	    #t)))


    ;; Update a key in the SMT, updating the root as well as inserting and deleting nodes as appropriate.
    ;; Outputs #t if successful and #f otherwise.
    (define (smt-update! smt key value)
      ;; Trace down
      (define key-bits ((smt-key->bits smt) key))
      (define-values (key-path siblings leaf-depth) (smt-trace smt key-bits))
      
      ;;  The terminal node must be a leaf with the same key-to-bit decomposition, else it is not present in the tree.
      (if (let ((terminal-node (smt-ref smt (car key-path))))
	    (and (leaf? terminal-node) (equal? key-bits ((smt-key->bits smt) (leaf-key terminal-node)))))
	  (let* ((new-leaf (leaf key value)) ; Compute new leaf
		 (new-path (smt-compute-path smt key-bits (- leaf-depth 1) new-leaf siblings))) ; Compute new path

	    ;; Commit new nodes to DB
	    (for-each (lambda (pair) (smt-set! smt (car pair) (cdr pair))) new-path)
	    
	    ;; Set new root
	    (smt-set-root! smt (caar new-path))
	    
	    ;; Delete nodes along former path
	    (for-each (lambda (hash) (smt-delete-node! smt hash)) key-path)
	    #t)
	  #f))

    ;; Delete a key from the SMT, updating the root as well as inserting and deleting nodes as appropriate.
    ;; Outputs #t if successful and #f otherwise.
    (define (smt-delete! smt key)
      ;; Trace down
      (define key-bits ((smt-key->bits smt) key))
      (define-values (key-path old-siblings leaf-depth) (smt-trace smt key-bits))

      (if (let ((terminal-node (smt-ref smt (car key-path))))
	    (and (leaf? terminal-node) (equal? key-bits ((smt-key->bits smt) (leaf-key terminal-node))))) ;; Only delete if the key is present
	  (begin
	    ;; Inspect final sibling to determine new branching post-deletion
	    (let-values (((new-leaf new-siblings new-branch-depth)
			  (if (null? old-siblings) ;; If there are no siblings, we are deleting a lone key.
			      (values #f '() 0)
			      (let ((final-sibling (smt-ref smt (car old-siblings))))
				(if (branch? final-sibling) ;; Branch means no compression
				    (values #f old-siblings (- leaf-depth 1))
				    (apply values (cons final-sibling ;; Must have a leaf here
							(let loop ((i (- leaf-depth 1))
								   (siblings (cdr old-siblings)))
							  (cond ((null? siblings) (list '() i))
								((car siblings) (list siblings i))
								(else (loop (- i 1) (cdr siblings))))))))))))
	      (let 
		  ;; Compute new path to leaf
		  ((new-path (smt-compute-path smt key-bits new-branch-depth new-leaf new-siblings)))
		
		;; Commit new nodes to DB, omitting the leaf node if it is empty.
		(for-each (lambda (pair) (maybe-map (lambda (node) (smt-set! smt (car pair) node)) (cdr pair)))
			  new-path)
		
		;; Set new root
		(smt-set-root! smt (and (cdar new-path) (caar new-path)))
		
		;; Delete nodes along former path
		(for-each (lambda (hash) (smt-delete-node! smt hash)) key-path)))
	    #t)
	  #f))))
