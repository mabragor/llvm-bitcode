
(in-package #:llvm-bitcode)

(cl-interpol:enable-interpol-syntax)
(quasiquote-2.0:enable-quasiquote-2.0)

(defmacro! define-enum (name &body specs)
  `(defparameter ,name (let ((,g!-res nil) (,g!-i -1))
			 ,@(mapcar (lambda (x)
				     (if (atom x)
					 `(push (cons ',x (incf ,g!-i))	,g!-res)
					 `(push (cons ',(car x) (setf ,g!-i ,(cadr x))) ,g!-res)))
				   specs)
			 (nreverse ,g!-res))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun error-block-parser (form)
    (llvm-read-error "Block ~a is unexpected in this context." (cdr (assoc 'block-id form))))
  (defun error-record-parser (form &optional type)
    (declare (ignore type))
    (llvm-read-error "Record ~a is unexpected in this context." (cdr (assoc 'code form))))
  (defun warn-block-parser (form)
    (warn "Block ~a is unexpected in this context -- I do standard parsing." (cdr (assoc 'block-id form)))
    (default-block-parser form))
  (defun warn-record-parser (form &optional type)
    (warn "Record ~a is unexpected in this context -- I do standard parsing." (cdr (assoc 'code form)))
    (default-record-parser form type))
  (defun skip-block-parser (form)
    (declare (ignore form))
    nil)
  (defun skip-record-parser (form &optional type)
    (declare (ignore form type))
    nil)

  (defparameter block-parser-stash (make-hash-table :test #'eq)))

(defun parse-string-field (x)
  (if (eq :array (car x))
      (coerce (cadr x) 'string)
      (error "Don't know how to parse this as string: ~a" x)))
      

(defun element-cons-parser-code (spec)
  (or (case (car spec)
	(function `(funcall ,spec (car cur)))
	(parse-with-function `(funcall ,(cadr spec) (car cur)))
	(parse-rest-with-function `(funcall ,(cadr spec) cur)))))
  
(defun element-atom-parser-code (spec)
  (cond ((eq 'int spec)  `(int<- (car cur)))
	((eq 'string spec) `(parse-string-field (car cur)))
	(t (error "Don't know how to understand this atom record parser spec: ~a" spec))))

(defun element-atom-advancer-code (spec)
  (declare (ignore spec))
  `(setf cur (cdr cur)))

(defun element-parser-code (spec)
  (if (atom spec)
      (element-atom-parser-code spec)
      (element-cons-parser-code spec)))

(defun element-advancer-code (spec)
  (if (atom spec)
      (element-atom-advancer-code spec)
      (element-cons-advancer-code spec)))

(defun element-cons-advancer-code (spec)
  (ecase (car spec)
    (function `(setf cur (cdr cur)))
    (parse-with-function `(setf cur (cdr cur)))
    (parse-rest-with-function nil)))
  
(defun mk-element-parser-code (spec)
  (declare (special side-effect))
  (if (atom spec)
      `(progn (push ,(element-atom-parser-code spec) res)
	      ,(element-atom-advancer-code spec))
      (if (eq :side-effect (car spec))
	  (progn (setf side-effect (cdr spec))
		 nil)
	  (let ((code (element-cons-parser-code spec)))
	    (if code
		`(progn (push ,code res)
			,(element-cons-advancer-code spec))
		`(progn (push (cons ,(intern (string (car spec)) "KEYWORD")
				    ,(element-parser-code (cadr spec)))
			      res)
			,(element-advancer-code (cadr spec))))))))

(defun! make-record-parser (specs name id)
  (declare (ignorable id))
  (let (side-effect)
    (declare (special side-effect))
    (let ((expanded-specs (mapcar #'mk-element-parser-code specs)))
      `(lambda (form &optional type)
	 (declare (ignorable type))
	 (let (res
	       (cur (cdr (assoc 'fields form))))
	   ;; TODO : check that lengths match
	   ,@expanded-specs
	   ,@side-effect
	   ;; (nreverse res)
	   (cons ,(intern (string name) "KEYWORD") res))))))

(defun find-cons-pos (tree sym)
  "Find first cons in TREE, whose CAR is EQ to SYM"
  (labels ((rec (x)
	     (if (consp x)
		 (if (eq sym (car x))
		     (return-from find-cons-pos x)
		     (progn (rec (car x))
			    (rec (cdr x)))))))
    (rec tree)
    (error "Was unable to find ~a in a tree ~a" sym tree)))

(defun if-around--wrap (specs body)
  (let ((around (find-if (lambda (x)
			   (and (consp x)
				(eq :around (car x))))
			 specs)))
    (if (not around)
	body
	(let ((res (copy-tree (cdr around))))
	  (setf (car (find-cons-pos res 'sub-body)) body)))))

(defun! make-block-parser-wrap (keys specs)
  (flet ((key (kwd) (getf keys kwd)))
    (let ((res (if-around--wrap specs `(let ((block-parsers (let ((it (make-hash-table :test #'equal)))
							      (setf (gethash 'default it) ,(if (key :on-undefined-blocks)
											       `#',(intern #?"$((key :on-undefined-blocks))-BLOCK-PARSER")
											       `#'default-block-parser))
							      ,@(mapcar (lambda (x)
									  `(setf (gethash ,(or (cdr (assoc x block-ids))
											       (error "I don't know ID for block ~a: check BLOCK-ID assoc list." x))
											  it)
										 (or (gethash ',x block-parser-stash)
										     (progn (warn "No custom parser is yet defined for block ~a -- using default parser." ',x)
											    (lambda (form)
											      (with-vanilla-parsers
												(default-block-parser form)))))))
									(cdr (assoc 'blocks specs)))
							      it))
					     (record-parsers (let ((it (make-hash-table :test #'equal)))
							       (setf (gethash 'default it) ,(if (key :on-undefined-records)
												`#',(intern #?"$((key :on-undefined-records))-RECORD-PARSER")
												`#'default-record-parser))
							       ,@(iter (with i = 0)
								       (for rec-spec in (cdr (assoc 'records specs)))
								       (destructuring-bind (name id) (if (atom (car rec-spec))
													 (list (car rec-spec) (incf i))
													 (list (caar rec-spec)
													       (setf i (cadar rec-spec))))
									 (collect `(setf (gethash ,id it)
											 ,(make-record-parser (cdr rec-spec) name id)))))
							       it)))
					 ,g!-sub-body))))
      (values res (find-cons-pos res g!-sub-body)))))



(defmacro! define-block (name (&rest keys) &body specs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name block-parser-stash)
	   (lambda (form)
	     (let ((res ,(multiple-value-bind (form sub-body) (make-block-parser-wrap keys specs)
					      ;; TODO : maybe there should be a room for side-effects at block level also?
					      (setf (car sub-body) `(default-block-parser form))
					      form)))
	       (cons ,(intern (string name) "KEYWORD")
		     (caddr res)))))))

(defmacro! define-toplevel-parser ((&rest keys) &body specs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash 'toplevel block-parser-stash)
	   (lambda ()
	     ,(multiple-value-bind (form sub-body) (make-block-parser-wrap keys specs)
				   ;; TODO : maybe there should be a room for side-effects at block level also?
				   (setf (car sub-body) `(default-toplevel-parser))
				   form)))))



    
