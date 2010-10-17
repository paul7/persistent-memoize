(defsystem #:persistent-memoize+restas
  :depends-on (#:postmodern #:cl-postgres+local-time #:iterate #:alexandria #:restas)
  :components ((:module "src"
			:components ((:file "package")
				     (:file "memoize"
					    :depends-on ("package"))
				     (:file "hash-backend"
					    :depends-on ("memoize"))
				     (:file "postgres-backend"
					    :depends-on ("memoize"))
				     (:file "restas"
					    :depends-on ("memoize"))))))
