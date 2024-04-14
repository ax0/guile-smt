(define-library (smt db)
  (export make-smt-db
	    smt-db?
	    smt-db-open
	    smt-db-get-root
	    smt-db-set-root!
	    smt-db-set!
	    smt-db-ref
	    smt-db-delete!
	    smt-db-close)
  (import
   (scheme base))
  (begin
;; An SMT db consists of:
;; - an open procedure,
;; - a put! procedure,
;; - a ref procedure,
;; - a delete! procedure and
;; - a close function.
;; All operations should have appropriate configuration options
;; embedded in them (examples are provided with sane defaults)
;; and the DB operations should handle types appropriately.
(define-record-type <smt-db>
  (make-smt-db open close get-root set-root! put! ref delete!)
  smt-db?
  (open smt-db-open)
  (get-root smt-db-get-root)
  (set-root! smt-db-set-root!)
  (put! smt-db-set!)
  (ref smt-db-ref)
  (delete! smt-db-delete!)
  (close smt-db-close))))
