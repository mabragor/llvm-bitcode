
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
  (records (string (parse-rest-with-function #'parse-string-field))
	   (epoch int)))

(define-toplevel-parser (:on-undefined-blocks :error :on-undefined-records :error)
  (blocks identification
	  module
	  ))

(defparameter use-relative-ids nil)
(defparameter attribute-groups nil)
(defparameter attributes-of-objects nil)
(defparameter attributes nil)
(defparameter type-table nil)
(defparameter module nil)
(defparameter section-table nil)
(defparameter comdat-table nil)
(defparameter value-table nil)
(defparameter global-inits nil)
(defparameter metadata-table nil)
(defparameter num-module-mds nil)
(defparameter seen-module-values-record nil)
(defparameter vst-offset nil)
(defparameter gc-table nil)
(defparameter comdat-table nil)
(defparameter comdat-map nil)

(defun populate-section-table (str)
  (nconc section-table (list str)))

;; TODO : numbers (codes) of blocks are defined elsewhere
;; TODO : global cleanup upon exit from this routine
(define-block module (:on-undefined-blocks :error) ; :on-repeat :error :on-undefined-records :error)
  (:around ;; here we write all the context-establishing things
   (let ((use-relative-ids nil)
	 (attribute-groups (make-hash-table :test #'equal))
	 (attributes-of-objects (make-hash-table :test #'equal))
	 (comdat-map (make-hash-table :test #'equal))
	 (attributes nil)
	 (type-table (make-array 0))
	 (metadata-table (make-array 0))
	 (num-module-mds 0)
	 (vst-offset 0)
	 module section-table value-table global-inits seen-module-values-record gc-table)
     sub-body))
  (blocks paramattr paramattr-group type value-symtab
	  constants metadata metadata-kind function use-list operand-bundle-tags)
  ;; Order of record specs is important, as this encodes their codes
  (records ((version 1) int
	    (:side-effect (cond ((equal 0 (car res)) (setf use-relative-ids nil))
				((equal 1 (car res)) (setf use-relative-ids t))
				(t (llvm-read-error "Unsupported LLVM bitcode version: ~a" (car res))))))
	   (target-triple (parse-rest-with-function #'parse-string-field)
			  (:side-effect (setf (getf module :target-triple) (car res))))
	   (datalayout (parse-rest-with-function #'parse-string-field)
		       (:side-effect (setf (getf module :datalayout) (car res))))
	   (asm (parse-rest-with-function #'parse-string-field)
		(:side-effect (setf (getf module :inline-asm) (car res))))
	   (section-name (parse-rest-with-function #'parse-string-field)
			 (:side-effect (populate-section-table (car res))))
	   (deplib (parse-rest-with-function #'parse-string-field))
	   ((global-var 7) (parse-rest-with-function #'parse-global-var)
	    (:side-effect (nconc value-table (list res))
			  (nconc global-inits (list (cons res (cdr (assoc :init-id res)))))))
	   ;; (function #'parse-function)
	   ;; (alias-old #'parse-alias-old)
	   ((purge-vals 10) int (:side-effect (setf value-table (subseq value-table 0 (car res)))))
	   (gc-name (parse-rest-with-function #'parse-string-field)
		    (:side-effect (nconc gc-table (list (car res)))))
	   (comdat (parse-rest-with-function #'parse-comdat)
		   ;; COMDATs are singletons, apparently.
	   	   (:side-effect (let ((it (get-or-insert-comdat (cdr (assoc :name res)))))
				   (setf (cdr (assoc :selection-kind it)) (cdr (assoc :selection-kind res)))
				   (nconc comdat-table (list it)))))
	   ((vst-offset 13) int (:side-effect (setf vst-offset (car res))))
	   ;; (alias #'parse-alias)
	   ;; TODO : + some additional consistency checks
	   ((metadata-values 15) int (:side-effect (setf num-module-mds (car res)
							 seen-module-values-record t
							 metadata-table (adjust-array metadata-table
										      num-module-mds
										      :initial-element nil))))
	   (source-filename (parse-rest-with-function #'parse-string-field)
			    (:side-effect (setf (getf module :source-fname) (car res))))))

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
     (defun ,name (x)
       (let ((it (assoc (int<- x) alist :test #'equal)))
	 (if it
	     (cdr it)
	     ,(let ((default (getf kwds :default)))
		   (if default
		       `(quote ,default)
		       `(llvm-read-error "Expected enum from ~a, but got ~a" alist x))))))))

(define-enum-parser parse-attribute-kind (:default none)
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

;; TODO : change to array here
(defun append-type-to-type-table (x)
  (declare (special type-table num-records))
  (if (aref type-table num-records)
      (llvm-read-error "Attempt to insert type ~a in place of something forward-referenced: ~a"
		       x (aref type-table num-records)))
  (setf (aref type-table num-records) x)
  (incf num-records)
  :success)

(let ((count 0))
  ;; TODO : somehow reset count between parses?
  (defun new-placeholder-struct ()
    (list :struct-placeholder (incf count))))

(defun get-type-by-id (id)
  (or (aref type-table id)
      (setf (aref type-table id) (new-placeholder-struct))))

(defun parse-pointer (lst)
  (destructuring-bind (elt-type &optional (addr-space 0)) lst
    (setf elt-type (get-type-by-id elt-type))
    (if (not (valid-element-type-p elt-type))
	(llvm-read-error "~a is not a valid type to be pointed at" elt-type))
    `(,elt-type (:addr-space . ,addr-space))))
	
(defparameter type-name nil)

(defun bool<- (x)
  (not (zerop (int<- x))))

(defun parse-struct-named (lst)
  (declare (special type-table num-records))
  (destructuring-bind (packed &rest elt-types) lst
    (setf packed (bool<- packed))
    (let ((res nil)
	  (it (aref type-table num-records)))
      (if (and it (eq :struct-placeholder (car it)))
	  (setf res it)
	  (setf res (new-placeholder-struct)
		(aref type-table num-records) res))
      (incf num-records)
      (setf (car res) :struct-named)
      ;; TODO : apparently, this does something strange w.r.t head of the list
      (nconc res (list (cons :name type-name)
		       (cons :packed packed)
		       (cons :elts (mapcar #'get-type-by-id elt-types))))
      (setf type-name nil)
      res)))
      
(defun parse-opaque (lst)
  (declare (ignore lst) (special type-table num-records type-name))
  (let ((res nil)
	(it (aref type-table num-records)))
    (if (and it (eq :struct-placeholder (car it)))
	(setf res it)
	(setf res (new-placeholder-struct)
	      (aref type-table num-records) res))
    (incf num-records)
    (setf (car res) :opaque)
    ;; TODO : apparently, this does something strange w.r.t head of the list
    (nconc res (list (cons :name type-name)))
    (setf type-name nil)
    res))

(defun valid-element-type-p (x)
  (declare (ignore x))
  t)

(defun valid-arg-type-p (x)
  (declare (ignore x))
  t)

(defun get-types-by-id (lst)
  (mapcar #'get-type-by-id lst))

(defun get-arg-types (lst)
  (mapcar (lambda (x)
	    (let ((it (get-type-by-id x)))
	      (if (valid-arg-type-p it)
		  it
		  (llvm-read-error "Type ~a is not valid argument type" it))))
	  lst))


(define-block type (:on-repeat :error :on-undefined-blocks :error) ; :on-undefined-records :error)
  ;; TODO : check for matching size of typetable
  (:around (let ((type-name nil)
		 (num-records 0))
	     (declare (special type-name num-records type-name))
	     sub-body))
  (records ((numentry 1) int
	    (:side-effect (setf type-table (adjust-array type-table (car res) :initial-element nil))))
	   (void)
	   (float)
	   (double)
	   (label)
	   (opaque (parse-rest-with-function #'parse-opaque))
	   ;; TODO : bounds checking for int
	   (integer int)
	   (pointer (parse-rest-with-function #'parse-pointer))
	   (function-old (vararg bool) (attr-id int) (ret-type #'get-type-by-id)
			 (arg-types (parse-rest-with-function #'get-arg-types)))
	   (half)
	   (array int #'get-type-by-id
		  (:side-effect (if (not (valid-element-type-p (cadr res)))
				    (llvm-read-error "~a is not a valid type for array element" res))))
	   (vector int #'get-type-by-id
		   (:side-effect (if (not (valid-element-type-p (cadr res)))
				     (llvm-read-error "~a is not a valid type for vector element" res))
				 (if (equal 0 (car res))
				     (llvm-read-error "Vector length can't be 0"))))
	   (x86-fp80)
	   (fp128)
	   (ppc-fp128)
	   (metadata)
	   (x86-mmx)
	   (struct-anon (packed bool) (elt-types (parse-rest-with-function #'get-types-by-id)))
	   (struct-name (parse-rest-with-function #'parse-string-field)
			(:side-effect (setf type-name (car res))))
	   (struct-named (parse-rest-with-function #'parse-struct-named))
	   ;; TODO : I need the ability to execute custom code, not only side-effect
	   (function (vararg bool) (ret-type #'get-type-by-id)
	   	     (arg-types (parse-rest-with-function #'get-arg-types)))
	   ((token 22))
	   ;; This side effect is *common* to all the records in this block
	   (:side-effect (if (not (find (car it) '(:numentry :struct-name :struct-named :opaque) :test #'eq))
			     (append-type-to-type-table it)))))

  
;; OK, let's manually go through this type-table block
#+nil
(:block 17 ; type-table block
  ((:record ((code . 1) (fields 12))) ;                                 numelts 12
   (:record ((code . 7) (fields 8))) ;                                  i8 -- index 0
   (:record ((code . 11) (abbrev-id . 9) (fields #*0001101 0))) ;       [13 x i8] -- index 1
   (:record ((code . 8) (abbrev-id . 4) (fields 1 0))) ;                [13 x i8]* (addrspace 0) -- index 2
   (:record ((code . 7) (fields 32))) ;                                 i32 -- index 3
   (:record ((code . 21) (abbrev-id . 5) (fields 0 (:array (3))))) ;    i32 () -- index 4
   (:record ((code . 8) (abbrev-id . 4) (fields 4 0))) ;                (i32 ())* -- index 5
   (:record ((code . 8) (abbrev-id . 4) (fields 0 0))) ;                i8* -- index 6
   (:record ((code . 21) (abbrev-id . 5) (fields 0 (:array (3 6))))) ;  i32 (i8*) -- index 7
   (:record ((code . 8) (abbrev-id . 4) (fields 7 0))) ;                (i32 (i8*))* -- index 8
   (:record ((code . 16) (fields))) ;                                   metadata -- index 9
   (:record ((code . 7) (fields 64))) ;                                 i64 -- index 10
   (:record ((code . 2) (fields))) ;                                    void -- index 11
   ))

(defun get-addr-space (type)
  (if (not (eq :pointer (car type)))
      (llvm-read-error "Attempt to get addrspace of not-pointer type: ~a" type))
  (or (cdr (find-if (lambda (x)
		      (and (consp x)
			   (eq :addr-space (car x))))
		    (cadr type)))
      0))

(defun get-element-type (type)
  (if (not (eq :pointer (car type)))
      (llvm-read-error "Attempt to get underlying type of not-pointer type: ~a" type))
  (or (caadr type)
      (llvm-read-error "Somehow underlying type of pointer type ~a is NIL" type)))

(defun parse-alignment (x)
  "Note from C++: Alignment in bitcode files is incremented by 1, so that zero
can be used for default alignment."
  ;; TODO : check for maximum alignment value
  (ash 1 (1- (int<- x))))


(defun parse-global-var (lst)
  (destructuring-bind (type pre-addr-space init-id raw-linkage
			    alignment section &optional (visibility 'default) thread-local
			    unnamed-addr external-init dll-storage comdat) lst
    (let (constant addr-space linkage)
      (setf type (get-type-by-id type))
      (when (not (zerop (boole boole-and pre-addr-space 1)))
	(setf constant t))
      (if (not (zerop (boole boole-and pre-addr-space 2)))
	  (setf addr-space (ash pre-addr-space -2))
	  (setf addr-space (get-addr-space type)
		type (get-element-type type)))
      (setf alignment (parse-alignment alignment)
	    linkage (decode-linkage raw-linkage)
	    visibility (if (eq 'local linkage)
			   (let ((it (decode-visibility visibility)))
			     (if (not (eq 'default it))
				 (llvm-read-error "Visibility for local linkage should be default, but got: ~a"
						  it)
				 it))
			   (decode-visibility visibility))
	    thread-local (decode-thread-local thread-local)
	    unnamed-addr (bool<- unnamed-addr)
	    external-init (bool<- external-init)
	    section (get-section-from-id section)
	    dll-storage (if dll-storage
			    (decode-dll-storage dll-storage)
			    (upgrade-import-export-linkage raw-linkage))
	    comdat (if comdat
		       (get-comdat-from-id comdat)
		       (if (has-implicit-comdat linkage)
			   1 ; TODO : I don't understand why this should be so
			   )))
      ;; TODO : change for insert kwd if nonnil or something
      `((:init-id . ,init-id) (:type . ,type) (:addr-space . ,addr-space) (:constant . ,constant)
	(:linkage . ,linkage) (:alignment . ,alignment)
	(:section . ,section) (:visibility . ,visibility) (:thread-local . ,thread-local)
	(:unnamed-addr . ,unnamed-addr) (:external-init . ,external-init)
	(:dll-storage . ,dll-storage) (:comdat . ,comdat)))))


(define-enum-parser decode-linkage (:default external)
  (external 0) weak-any (appending 2) internal link-once-any
  (external 5) external ; yep, it's not one-to-one
  external-weak common private weak-odr link-once-odr (available-externally 12)
  private private external weak-any weak-odr link-once-any
  link-once-odr)

(define-enum-parser decode-visibility (:default default)
  (default 0) hidden protected)

(define-enum-parser decode-thread-local (:default general-dynamic)
  (not-thread-local 0) general-dynamic local-dynamic initial-exec local-exec)

(define-enum-parser decode-dll-storage (:default default)
  (default 0) import export)

(define-enum-parser upgrade-import-export-linkage ()
  (import 5) export)

(defun get-section-from-id (id)
  (if (not (zerop id))
      (or (nth (1- id) section-table)
	  (llvm-read-error "Section table index out of bounds: ~a" (1- id)))))

(defun get-comdat-from-id (id)
  (if (not (zerop id))
      (or (nth (1- id) comdat-table)
	  (llvm-read-error "Comdat table index out of bounds: ~a" (1- id)))))

(defun has-implicit-comdat (linkage)
  (member linkage '(weak-any link-once-any weak-odr-linkage link-once-odr)))
      
(define-enum-parser decode-selection-kind ()
  (any 1) exact-match largest no-duplicates same-size)

(defun parse-comdat (lst)
  (destructuring-bind (selection-kind size &rest name) lst
    (setf selection-kind (decode-selection-kind selection-kind)
	  name (parse-string-field (subseq name 0 size)))
    `((:name . ,name) (:selection-kind . ,selection-kind))))

(defun get-or-insert-comdat (name)
  (or (gethash name comdat-map)
      (setf (gethash name comdat-map)
	    (list (cons :name name) (cons :selection-kind :unknown)))))
      
(define-block constants (:on-undefined-blocks :error :on-undefined-records :error)
  (records ((settype 1) ...)
	   (null ...)
	   (undef ...)
	   (integer ...)
	   (wide-integer ...)
	   (float ...)
	   (aggregate ...)
	   (string ...)
	   (cstring ...)
	   (ce-binop ...)
	   (ce-cast ...)
	   (ce-gep ...)
	   (ce-select ...)
	   (ce-extractelt ...)
	   (ce-insertelt ...)
	   (ce-shufflevec ...)
	   (ce-cmp ...)
	   (inlineasm-old ...)
	   (ce-shufvec-ex ...)
	   (ce-inbounds-gep ...)
	   (blockaddress ...)
	   (data ...)
	   (inlineasm ...)))

