(in-package #:persistent-memoize)

(defgeneric memoize/impl (storage key value &key expire-at))

(defgeneric get-memoized-value/impl (storage key))

(defgeneric remove-memoized-value/impl (storage key))

(defgeneric clear-memoized-values/impl (storage))

(defmethod memoize/impl ((storage (eql nil)) key value &key expire-at)
  (declare (ignore key expire-at))
  value)

(defmethod get-memoized-value/impl ((storage (eql nil)) key)
  (declare (ignore key))
  (values nil nil))

(defmethod remove-memoized-value/impl ((storage (eql nil)) key)
  (declare (ignore key)))

(defmethod clear-memoized-values/impl ((storage (eql nil))))

(defvar *storage* nil)

(defmacro memoize (key value &key expire-at)
  `(memoize/impl *storage* ,key ,value :expire-at ,expire-at))

(defmacro get-memoized-value (key)
  `(get-memoized-value/impl *storage* ,key))

(defmacro remove-memoized-value (key)
  `(remove-memoized-value/impl *storage* ,key))

(defmacro clear-memoized-values ()
  `(clear-memoized-values/impl *storage*))

(defmacro with-memoization (storage &body body)
  `(let ((*storage* ,storage))
     ,@body))

(defmacro memoize-with-key (key &body body)
  (with-gensyms (value present-p)
    `(multiple-value-bind (,value ,present-p) (get-memoized-value ,key)
       (if ,present-p
	   ,value
	   (memoize ,key (progn 
			   ,@body))))))

(defmacro define-memoized-function (name args &body body)
  (with-gensyms (key)
    `(defun ,name ,args
       (let ((,key (list ',name ,@args)))
	 (memoize-with-key ,key 
	   ,@body)))))

