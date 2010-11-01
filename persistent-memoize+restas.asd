(defsystem #:persistent-memoize+restas
  :depends-on (#:persistent-memoize #:restas)
  :components ((:module "src"
			:components ((:file "restas")))))
