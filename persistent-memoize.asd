(defsystem #:persistent-memoize
  :depends-on (#:postmodern #:cl-postgres+local-time #:iterate #:alexandria)
  :components ((:module "src"
			:components ((:file "package")
				     (:file "memoize")
				     (:file "hash-backend")
				     (:file "postgres-backend")))))
