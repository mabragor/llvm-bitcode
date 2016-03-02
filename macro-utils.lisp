
(in-package #:llvm-bitcode)

(defmacro! define-enum (name &body specs)
  `(defparameter ,name (let ((,g!-res nil) (,g!-i -1))
			 ,@(mapcar (lambda (x)
				     (if (atom x)
					 `(push (cons ',x (incf ,g!-i))	,g!-res)
					 `(push (cons ',(car x) (setf ,g!-i ,(cadr x))) ,g!-res)))
				   specs)
			 (nreverse ,g!-res))))
