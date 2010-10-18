(in-package #:persistent-memoize)

(defun make-hashtable-storage ()
  (make-hash-table :test #'equal))

(defmethod memoize/impl ((storage hash-table) key value &key expire-at)
  (setf (gethash key storage) (cons value expire-at))
  value)

(defmethod get-memoized-value/impl ((storage hash-table) key)
  (multiple-value-bind (data found) (gethash key storage)
    (if found
	(destructuring-bind (value . expire-at) data
	  (if (and expire-at (timestamp< expire-at (now)))
	      (progn
		(remhash key storage)
		(values nil nil))
	      (values value found))))))

(defmethod remove-memoized-value/impl ((storage hash-table) key)
  (remhash key storage))

(defmethod clear-memoized-values/impl ((storage hash-table))
  (clrhash storage))
