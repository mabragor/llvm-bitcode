
(in-package llvm-bitcode)

(cl-interpol:enable-interpol-syntax)

;; OK, let's gradually grow this file, so that at each stage we can interactively test
;; what we add.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-enum block-ids
    (block-info 1) (module first-application-block-id) paramattr paramattr-group constants function
    identification value-symtab metadata metadata-attachment
    type use-list module-strtab function-summary operand-bundle-tags
    metadata-kind))

;; Maybe ERRORing is the default behavior -- I shouldn't explicitly write it down
(define-block identification (:on-undefined-blocks :error :on-undefined-records :error)
  (records (string string)
	   (epoch int)))

(define-toplevel-parser (:on-undefined-blocks :error :on-undefined-records :error)
  (blocks identification
	  module
	  ))

(defparameter use-relative-ids nil)
(defparameter attribute-groups nil)
(defparameter attributes-of-objects nil)
(defparameter attributes nil)

;; TODO : numbers (codes) of blocks are defined elsewhere
;; TODO : global cleanup upon exit from this routine
(define-block module (:on-undefined-blocks :error) ; :on-repeat :error :on-undefined-records :error)
  (:around ;; here we write all the context-establishing things
   (let ((use-relative-ids nil)
	 (attribute-groups (make-hash-table :test #'equal))
	 (attributes-of-objects (make-hash-table :test #'equal))
	 (attributes nil))
     sub-body))
  (blocks paramattr paramattr-group type value-symtab
	  constants metadata metadata-kind function use-list operand-bundle-tags)
  ;; Order of record specs is important, as this encodes their codes
  (records ((version 1) int
	    (:side-effect (cond ((equal 0 (car res)) (setf use-relative-ids nil))
				((equal 1 (car res)) (setf use-relative-ids t))
				(t (llvm-read-error "Unsupported LLVM bitcode version: ~a" (car res))))))
	   ))
	   ;; (target-triple string) ; TODO : what does module->setTargetTriple do?
	   ;; (datalayout string)
	   ;; (asm string)
	   ;; (section-name string (:side-effect (push (car res) section-table)))
	   ;; (deplib string)	   
	   ;; (global-var #'parse-global-var)
	   ;; (function #'parse-function)
	   ;; (alias-old #'parse-alias-old)
	   ;; (purge-vals int (:side-effect (setf value-list (subseq value-list 0 (car res)))))
	   ;; (gc-name string (:side-effect (push (car res) gc-table)))
	   ;; (comdat (selection-kind (parse-with-function
	   ;; 			    (lambda-enum-parser 1 (any exact-match largest no-duplicates same-size))))
	   ;; 	   (name (parse-rest-with-function #'lengthed-string))
	   ;; 	   ;; TODO : in C++ there are more side-effects. Should I implement them?
	   ;; 	   (:side-effect (push res comdat-list)))
	   ;; (vst-offset int (:side-effect (setf vst-offset (car res))))
	   ;; (alias #'parse-alias)
	   ;; ;; TODO : + some additional consistency checks
	   ;; (metadata-values int (:side-effect (set metadata-list (make-list (car res) nil))))
	   ;; (source-filename string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-enum-alist (specs)
    (iter (with i = 0)
	  (for spec in specs)
	  (if (atom spec)
	      (collect (cons (incf i) spec))
	      (collect (cons (setf i (cadr spec)) (car spec)))))))

(defmacro define-enum-parser (name kwds &body enums)
  ;; TODO : this should expand into parser definition
  `(let ((alist ',(make-enum-alist enums)))
     (defun ,(intern #?"PARSE-$(name)") (x)
       (let ((it (assoc (int<- x) alist :test #'equal)))
	 (if it
	     (cdr it)
	     ,(let ((default (getf kwds :default)))
		   (if default
		       `(quote ,default)
		       `(llvm-read-error "Expected enum from ~a, but got ~a" alist x))))))))

(define-enum-parser attribute-kind (:default none)
  (alignment 1) always-inline by-val inline-hint in-reg
  min-size naked nest no-alias no-builtin no-capture
  no-duplicate no-implicit-float no-inline non-lazy-bind
  no-red-zone no-return no-unwind optimize-for-size
  read-none read-only returned returns-twice s-ext
  stack-alignment stack-protect stack-protect-req
  stack-protect-strong struct-ret sanitize-address
  sanitize-thread sanitize-memory uw-table z-ext
  builtin cold optimize-none in-alloca non-null
  jump-table dereferenceable dereferenceable-or-null
  convergent safestack argmemonly swift-self swift-error
  no-recurse inaccessiblemem-only inaccessiblemem-or-argmemonly)

(defun ensure-one-of-syms (sym-bag x)
  (if (not (symbolp x))
      (llvm-read-error "Expected one of ~a, but got non-symbol ~a" sym-bag x)
      (if (not (member x sym-bag :test #'eq))
	  (llvm-read-error "Expected one of ~a, but got ~a" sym-bag x)
	  x)))

(defun parse-out-null-terminated-string (lst)
  (let ((lst lst) ; our own copy of lst
	res)
    (iter (for elt on lst)
	  (if (equal 0 (car elt))
	      (progn (setf lst elt)
		     (terminate))
	      (push (code-char (car elt)) res)))
    (if (not (equal 0 (car lst)))
	(llvm-read-error "The string is not null-terminated"))
    (values (coerce (nreverse res) 'string)
	    (cdr lst))))

(defun parse-string-with-val-attr (lst)
  (multiple-value-bind (str1 lst) (parse-out-null-terminated-string lst)
    (multiple-value-bind (str2 lst) (parse-out-null-terminated-string lst)
      (values str1 str2 lst))))


(defmacro lambda-enum-parser (&rest specs)
  `(let ((alist ',(make-enum-alist specs)))
     (lambda (x)
       (let ((it (assoc (int<- x) alist :test #'equal)))
	 (if it
	     (cdr it)
	     (llvm-read-error "Expected enum from ~a, but got ~a" alist x))))))
     

(defun parse-attrs-lst (lst)
  (if lst
      (ecase (funcall (lambda-enum-parser (enum 0) int (str 3) str-with-val) (car lst))
	(enum (cons (parse-attribute-kind (cadr lst))
		    (parse-attrs-lst (cddr lst))))
	(int (cons (cons (ensure-one-of-syms '(alignment stack-alignment dereferenceable dereferenceable-or-null)
					     (parse-attribute-kind (cadr lst)))
			 (int<- (caddr lst)))
		   (parse-attrs-lst (cdddr lst))))
	(str (multiple-value-bind (str rest) (parse-out-null-terminated-string (cdr lst))
	       (cons (cons str nil)
		     (parse-attrs-lst rest))))
	(str-with-val (multiple-value-bind (str val rest) (parse-string-with-val-attr (cdr lst))
			(cons (cons str val)
			      (parse-attrs-lst rest)))))))

(defun add-attr-group-to-current-context (grp-id obj-id attrs)
  (let ((it (gethash grp-id attribute-groups)))
    (if it
	(llvm-read-error "Attempt to redefine attribute group with id ~a" grp-id)
	(setf (gethash grp-id attribute-groups)
	      (list (cons :attrs attrs) (cons :object-index obj-id))))
    ;; (push grp-id (gethash obj-id attributes-of-objects))
    ))

(define-block paramattr-group (:on-repeat :error :on-undefined-blocks :error) ; :on-undefined-records :skip)
  ;; Maybe, there's better way to write 3 instead of hardcoding?
  (records ((grp-entry 3) (id int) (index int) (attrs (parse-rest-with-function #'parse-attrs-lst))
	    (:side-effect (add-attr-group-to-current-context (cdr (assoc :id res))
	    						     (cdr (assoc :index res))
	    						     (cdr (assoc :attrs res)))))
	   ))

(defun parse-attrs-new (lst)
  (mapcar (lambda (x)
	    (if (not (gethash x attribute-groups))
		(llvm-read-error "Attribute group ~a is not defined in current context." x))
	    (list :group x))
	  lst))

(defun append-attrs-to-current-context (lst)
  (nconc attributes (list lst)))

(define-block paramattr (:on-repeat :error :on-undefined-blocks :error :on-undefined-records :error)
  (records ;; TODO : actually, enable also the old format of entries
   ;; (entry-old #'parse-attrs-old
	   ;; 	      (:side-effect (append-attrs-to-current-context res)))
   ((entry 2) (parse-rest-with-function #'parse-attrs-new)
    (:side-effect (append-attrs-to-current-context res))
    )))

