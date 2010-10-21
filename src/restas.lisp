(in-package #:persistent-memoize)

(defmacro define-memoized-route (name args &body body)
  (with-gensyms (key value present-p memoize expire-at absolute)
    `(restas:define-route ,name ,args
       (let ((,key (hunchentoot:request-uri*)))
	 (multiple-value-bind (,value ,present-p) (get-memoized-value ,key)
	   (if ,present-p
	       ,value
	       (multiple-value-bind (,memoize ,expire-at ,absolute) (progn 
								      ,@body)
		 (if (or (not ,expire-at)
			 ,absolute)
		     (list :memoize ,memoize
			   :expire-at ,expire-at)
		     (list :memoize ,memoize
			   :expire-at (apply #'timestamp+ (now) ,expire-at))))))))))

(defun property-list-p (object)
  (and (listp object)
       (evenp (length object))))

(defmethod restas:render-object :around (designer object)
  (if (property-list-p object)
      (let ((memoize   (getf object :memoize))
	    (expire-at (getf object :expire-at)))
	(if memoize
	    (let ((key (hunchentoot:request-uri*)))
	      (memoize key (restas:render-object designer memoize)
		       :expire-at expire-at))
	    (call-next-method designer object)))
      (call-next-method designer object)))

(export 'define-memoized-route)
