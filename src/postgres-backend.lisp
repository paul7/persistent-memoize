(in-package #:persistent-memoize)

(defparameter *cleanup-after-operations* 100)

(defparameter *default-db-spec* '("zlodb" "lisp" "lisp" "localhost" :pooled-p t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '#:postmodern)
  (local-time:set-local-time-cl-postgres-readers))

(defclass storage-entry ()
  ((id         :col-type   serial
	       :accessor   storage-entry-id)
   (key        :col-type   text
	       :initarg    :key
	       :accessor   storage-entry-key)
   (value      :col-type   text
	       :initarg    :value
	       :accessor   storage-entry-value)
   (expire-at  :col-type   (or timestamp db-null)
	       :initarg    :expire-at
	       :accessor   storage-entry-expire-at))
  (:keys id)
  (:metaclass dao-class))

(defun install (&optional (db-spec *default-db-spec*))
  (with-connection db-spec
    (execute (dao-table-definition 'storage-entry))
    (execute (:create-index 'storage-key-index :on :storage-entry
			    :fields :key))))

(defun uninstall (&optional (db-spec *default-db-spec*))
  (with-connection db-spec
    (if (yes-or-no-p "This operation will purge all data. Proceed?")
	(execute (:drop-table 'storage-entry)))
    (values)))

(defclass storage ()
  ((db-spec    :accessor storage-db-spec
	       :initarg  :db-spec)
   (operations :accessor storage-operations
	       :initform 0)))

(defmacro with-storage (storage &body body)
  `(with-connection (storage-db-spec ,storage)
     ,@body))

(defun make-db-storage (&optional (db-spec *default-db-spec*))
  (make-instance 'storage :db-spec db-spec))

(export 'make-db-storage)

(defmethod memoize/impl ((storage storage) key value &key expire-at)
  (let ((entry (make-instance 'storage-entry 
			      :key       key
			      :value     value
			      :expire-at (or expire-at
					     :null))))
    (with-storage storage
      (insert-dao entry)
      (clear-expired storage)
      value)))

(defmethod get-memoized-value/impl ((storage storage) key)
  (with-storage storage
    (let ((entry (first
		  (select-dao 'storage-entry (:and (:= 'key key)
						   (:or (:is-null 'expire-at)
							(:> 'expire-at (now))))
			      (:desc 'id)))))
      (if entry
	  (values (storage-entry-value entry)
		  t)))))
  
(defmethod remove-memoized-value/impl ((storage storage) key)
  (with-storage storage 
    (query (:delete-from 'storage-entry :where (:= 'key key)))))

(defmethod clear-memoized-values/impl ((storage storage))
  (with-storage storage 
    (query (:delete-from 'storage-entry))))

(defun clear-expired (storage &key force)
  (if (or force
	  (> (incf (storage-operations storage)) *cleanup-after-operations*))
      (with-storage storage 
	(query (:delete-from 'storage-entry :where (:< 'expire-at (now))))
	(setf (storage-operations storage) 0))))
