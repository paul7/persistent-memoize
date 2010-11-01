(defsystem #:persistent-memoize+postmodern
  :depends-on (#:persistent-memoize #:postmodern #:cl-postgres+local-time)
  :components ((:module "src"
			:components ((:file "postgres-backend")))))
