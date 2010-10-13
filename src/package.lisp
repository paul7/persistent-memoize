(defpackage #:persistent-memoize
  (:nicknames #:pm)
  (:use #:cl #:postmodern #:local-time #:iterate #:alexandria)
  (:export))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (local-time:set-local-time-cl-postgres-readers))
