(defpackage #:persistent-memoize
  (:nicknames #:pm)
  (:use #:cl #:local-time #:iterate #:alexandria)
  (:export #:memoize
	   #:get-memoized-value
	   #:remove-memoized-value
	   #:clear-memoized-values
	   #:*storage*
	   #:with-memoization
	   #:define-memoized-function
	   #:make-hashtable-storage))
