(defpackage #:persistent-memoize
  (:nicknames #:pm)
  (:use #:cl #:postmodern #:local-time #:iterate #:alexandria)
  (:export #:memoize
	   #:get-memoized-value
	   #:remove-memoized-value
	   #:clear-memoized-values
	   #:*storage*
	   #:with-memoization
	   #:define-memoized-function
	   #:make-hashtable-storage))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (local-time:set-local-time-cl-postgres-readers))
