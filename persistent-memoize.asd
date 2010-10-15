(defsystem #:persistent-memoize
  :depends-on (#:postmodern #:cl-postgres+local-time #:iterate #:alexandria)
  :components ((:module "src"
			:components ((:file "package")
				     (:file "memoize"
					    :depends-on ("package"))
				     (:file "hash-backend"
					    :depends-on ("memoize"))
				     (:file "postgres-backend"
					    :depends-on ("memoize"))))))
