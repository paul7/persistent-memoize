(in-package #:persistent-memoize)

(defparameter *cleanup-after-operations* 100)

(defparameter *db-spec* '("zlodb" "lisp" "lisp" "localhost" :pooled-p t))

(defclass storage-entry ()
  ((id :col-type serial
       :accessor storage-entry-id)
   (key :col-type text
	:initarg :key
	:accessor storage-entry-key)
   (value :col-type text
	  :initarg :value
	  :accessor storage-entry-value)
   (operation# :initform 0
	       :accessor storage-operation#
	       :allocation :class))
  (:keys id)
  (:metaclass dao-class))

(defun install ()
  (with-connection *db-spec* 
    (execute (dao-table-definition 'storage-entry))
    (execute (:create-index 'storage-key-index :on :storage-entry
			    :fields :key))))

(defun uninstall ()
  (with-connection *db-spec*
    (if (yes-or-no-p "This operation will purge all data. Proceed?")
	(execute (:drop-table 'storage-entry)))
    (values)))

