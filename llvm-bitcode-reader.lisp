
;;;; llvm-bitcode-reader.lisp

(in-package #:llvm-bitcode)

(quasiquote-2.0:enable-quasiquote-2.0)
(cl-interpol:enable-interpol-syntax)

;; TODO : somehow I should restrict only to LLVM IR namespace

;; OK, let's first just copy all the enums we have in the LLVMBitCodes.h
;; and then see, how to write it in a more convenient form.

;; TODO : this one should start from FIRST-APPLICATION-BLOCK-ID
(defparameter block-ids '(module paramattr paramattr-group constants function
			  identification value-symtab metadata metadata-attachment
			  type use-list module-strtab function-summary operand-bundle-tags
			  metadata-kind))

;; TODO : these all are 1-based
(defparameter identification-codes '(string epoch))
(defparameter module-codes '(version triple datalayout asm section-name
			     deplib global-var function alias
			     purge-vals gcname))
(defparameter attribute-groups '(entry-old entry grp-entry))
(defparameter type-codes '(numentry void float double label opaque
			   integer pointer function-old half array vector
			   x86-fp80 fp128 ppc-fp128 metadata x86-mmx
			   struct-anon struct-name struct-named
			   function))
(defparameter type-symtab-codes '(entry))
(defparameter value-symtab-codes '(entry bb-entry))
(defparameter metadata-codes '(string _ _ name _ kind _
			       node fn-node named-node attachment))
(defparameter constant-codes '(set-type null undef integer wide-integer
			       float aggregate string cstring ce-binop
			       ce-cast ce-gep ce-select ce-extractelt
			       ce-insertelt ce-shufflevec ce-cmp
			       inline-asm-old
			       ce-shufvec-ex ce-inbounds-gep
			       block-address data inline-asm))

;; TODO : these are 0-based
(defparameter cast-opcodes '(trunc zext sext fp-to-ui fp-to-si ui-to-fp si-to-fp
			     fp-trunc fp-ext ptr-to-int int-to-ptr bit-cast))
(defparameter binary-opcodes '(add sub mul udiv sdiv urem srem shl lshr ashr
			       and or xor))
(defparameter rmw-operations '(xchg add sub and nand or xor max min umax umin))
(defparameter overflowing-flags '(no-unsigned-wrap no-signed-wrap))
(defparameter possibly-extract-flags '(extract))
(defparameter atomic-ordering-codes '(nonatomic unordered monotonic acquire
				      release acqrel seqcst))
(defparameter atomic-synch-scope-codes '(single-thread cross-thread))

;; TODO : these are 1-based
(defparameter function-codes '(declare-blocks binop cast gep select
			       extractelt insertelt shufflevec cmp
			       ret br switch invoke
			       _
			       unreachable phi
			       _ _
			       alloca load
			       _ _
			       vaarg store
			       _
			       extractval insertval cmp2
			       vselect inbounds-gep indirect-br
			       _
			       debug-loc-again call debug-loc fence cmpxchg
			       atomic-rmw resume landing-pad load-atomic store-atomic))
(defparameter uselist-codes '(entry))

;; (defun string<- (lst)
;;   (coerce (mapcar #'char-code lst) 'string))

;; (define-record version 1)
;; (define-record triple 2 string)
;; (define-record data-layout 3 string)
;; (define-record asm 4
;;   (cl-ppcre:split "\\n" (string<- x)))
;; ;; TODO : exactly one section-name per something???
;; (define-record section-name 5 string)
;; (define-record deplib 6 string) ; I've no clue what this is...

;; (define-record global-var 7
;;   ;; complicated definition
;;   ...)

;; (define-record function 8
;;   ;; compicated definition
;;   ...)

;; (define-record alias 9
;;   ;; complicated definition
;;   ...)

;; ;; TODO : after this level of parsing, there is another one, which
;; ;;      : actually uses these PURGE-VALS (and other context-altering things)
;; (define-record purge-vals 10)
;; (define-record gc-name 11 string)


;; (define-block param-attr 9
;;   (records entry))

;; Blocks to write (symbolic) parsers for
;; * module
;; * (done, modulo macros) paramattr
;; * (done, modulo macros) paramattr-group
;; * constants
;; * function
;; * (done) identification
;; * value-symtab
;; * metadata
;; * metadata-attachment
;; * type
;; * use-list
;; * module-strtab
;; * function-summary
;; * operand-bundle-tags
;; * metadata-kind

;; TODO : in the actual C++ LLVM code they put a lot of efforts to be able
;;      : to read parts of Bitcode file, to skip over chunks of it efficiently
;;      : and so on. Later, perhaps, this should be implemented too?

;; I should have an enclosing context, where all the block numbers are defined together

;; Maybe ERRORing is the default behavior -- I shouldn't explicitly write it down
(define-block identification (:on-undefined-blocks :error :on-undefined-records :error)
  (records (string string)
	   (epoch int)))

(defmacro define-enum-parser (name base-index &body kwds)
  ;; TODO : this should expand into parser definition
  `(defun parse-... (x)
     )
  nil)

(define-enum-parser attribute-kind (1 :default none)
  alignment always-inline by-val inline-hint in-reg
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
	      (push (car elt) res)))
    (if (not (equal 0 (car lst)))
	(llvm-read-error "The string is not null-terminated"))
    (values (coerce (nreverse res) 'string)
	    (cdr lst))))

(defun parse-string-with-val-attr (lst)
  (multiple-value-bind (str1 lst) (parse-out-null-terminated-string lst)
    (multiple-value-bind (str2 lst) (parse-out-null-terminated-string lst)
      (values str1 str2 lst))))

(defun parse-attrs-lst (lst)
  (if lst
      (ecase (funcall (lambda-enum-parser 0 (enum int str str-with-val)) (car lst))
	(enum (cons (parse-attribute-kind (cadr lst))
		    (parse-attrs-lst (cddr lst))))
	(int (cons (cons (ensure-one-of-syms '(alignment stack-alignment dereferenceable dereferenceable-or-null)
					     (parse-attribute-kind (cadr lst)))
			 (int<- (caddr lst)))
		   (parse-attrs-lst (cdddr lst))))
	(str (multiple-value-bind (str rest) (parse-out-null-terminated-string lst)
	       (cons (cons str nil)
		     (parse-attrs-lst rest))))
	(str-with-val (multiple-value-bind (str val rest) (parse-string-with-val-attr lst)
			(cons (cons str val)
			      (parse-attrs-lst rest)))))))
			
    
    

(define-block paramattr-group (:on-repeat :error :on-undefined-blocks :skip :on-undefined-records :skip)
  ;; Maybe, there's better way to write 3 instead of hardcoding?
  (records ((grp-entry 3) (id int) (index int) (attrs (parse-rest-with-function #'parse-attrs-lst))
	    (:side-effect (add-attr-group-to-current-context (cdr (assoc 'id res))
							     (cdr (assoc 'index res))
							     (cdr (assoc 'attrs res)))))))

(defun parse-attrs-new (lst)
  ;; TODO : check that current group id is actually present in the current context?
  (mapcar (lambda (x)
	    (list :group x))
	  lst))


(defun parse-raw-attrs (x)
  (let ((attrs1 (funcall (lambda-enum-power-parser
			  0 (z-ext s-ext no-return in-reg struct-ret no-unwind no-alias by-val next read-none
				   read-only no-inline always-inline optimize-for-size stack-protect
				   stack-protect-req))
			 x))
	(attrs2 (funcall (lambda-enum-power-parser
			  21 (no-capture no-red-zone no-implicit-float naked inline-hint))
			 x))
	(attrs3 (funcall (lambda-enum-power-parser
			  29 (returns-twice uw-table non-lazy-bind sanitize-address min-size
					    no-duplicate stack-protect-strong sanitize-thread
					    sanitize-memory no-builtin returned cold builtin
					    optimize-none in-alloca non-null jump-table
					    convergent safe-stack no-recurse inaccessible-mem-only
					    inaccessible-mem-or-arg-mem-only))
			 x)))
    (let ((pre-alignment (int<- (rshift (bit-and (bits<- (lshift (bits<- 31) 16) 64) x) 16)))
	  (pre-stack-alignment (int<- (rshift (bit-and (bits<- (lshift (bits<- 7) 26) 64) x) 26))))
      (let ((res (nconc attrs1 attrs2 attrs3)))
	(when (not (zerop pre-alignment))
	  (push (cons 'alignment (expt 2 (1- pre-alignment)))
		res))
	(when (not (zerop pre-stack-alignment))
	  (push (cons 'stack-alignment (expt 2 (1- pre-stack-alignment)))
		res))
	res))))

(defun decode-llvm-attrs-old (x)
  "The (apparently) legacy way to store parameter attributes."
  (let ((x (bits<- x 64)))
    (let ((alignment (int<- (rshift (bit-and (bits<- (lshift (bits<- "ffff") 16) 64) x) 16))))
      (let ((rest (parse-raw-attrs (bit-ior (bits<- (rshift (bit-and (bits<- (lshift (bits<- "fffff") 32) 64)
								     x)
							    11)
						    64)
					    (bit-and (bits<- "ffff" 64) x)))))
	(if (not (zerop alignment))
	    (cons (cons 'alignment alignment)
		  rest)
	    rest)))))

(defun parse-attrs-old (lst)
  (iter (generate elt in lst)
	(collect (list (next elt)
		       (decode-llvm-attrs-old (next elt))))))
	  
(define-block paramattr (:on-repeat :error :on-undefined-blocks :skip :on-undefined-records :skip)
  (records (entry-old #'parse-attrs-old
		      (:side-effect (append-attrs-to-current-context res)))
	   (entry #'parse-attrs-new
		  (:side-effect (append-attrs-to-current-context res)))))

(defun %parse-alias (x &optional (old-p t))
  (let ((it (mk-iter x)))
    (macrolet ((next! () `(inext-or-error it)))
      (let* ((type (type-by-id (next!)))
	     (addr-space (if old-p
			     ;; this is a really kludgy code
			     (let ((it (get-addr-space type)))
			       (setf type (get-element-type type))
			       it)
			     (next!)))
	     (val (next!))
	     (linkage (decode-linkage (next!)))
	     (res `((:type . ,type) (:addr-space . ,addr-space) (:val . ,val) (:linkage . ,linkage))))
	;; now here is the part with optional arguments (the new alias style)
	(handler-case (progn (let ((visibility (decode-visibility (next!))))
			       (if (and (eq 'local (cdr (assoc :linkage res)))
					(not (eq 'default visibility)))
				   (llvm-read-error "Alias with local linkage should have default visibility, ~
                                                     but got ~a" visibility))
			       (push (cons :version version) res))
			     (handler-case (push (cons :dll-storage-class (decode-dll-storage-class (next!))) res)
			       (stop-iteration () (progn (upgrade-dll-import-export-linkage res)
							 (error 'stop-iteration))))
			     (push (cons :thread-local-mode (decode-thread-local (next!))) res)
			     (push (cons :unnamed-addr (next!)) res))
	  (stop-iteration () nil))
	(push res value-list)
	(push (cons res (cdr (assoc :val res))) alias-inits)
	res))))
      
(defun parse-alias (x)
  (%parse-alias x))
(defun parse-alias-old (x)
  (%parse-alias x t))

(defun parse-global-var (x)
  (destructuring-bind (type pre-addr-space init-id raw-linkage
			    alignment section &optional (visibility 'default) thread-local
			    unnamed-addr external-init dll-storage comdat) x
    (let (constant addr-space linkage)
      (setf type (type-by-id type))
      (when (not (zerop (boole boole-and pre-addr-space 1)))
	(setf constant t))
      (if (not (zerop (boole boole-and pre-addr-space 1)))
	  (setf addr-space (ash it -2))
	  (setf addr-space (get-addr-space type)
		type (get-element-type type)))
      (setf alignment (parse-alignment aligment)
	    linkage (decode-linkage raw-linkage)
	    visibility (if (eq 'local linkage)
			   (let ((it (decode-visibility visibility)))
			     (if (not (eq 'default it))
				 (llvm-read-error "Visibility for local linkage should be default, but got: ~a"
						  it)
				 it))
			   (decode-visibility visibility))
	    thread-local (decode-thread-local thread-local)
	    unnamed-addr (decode-binary unnamed-addr)
	    external-init (decode-binary external-init)
	    section (get-section-from-id section)
	    dll-storage (if dll-storage
			    (decode-dll-storage dll-storage)
			    (upgrade-import-export-linkage raw-linkage))
	    comdat (if comdat
		       (get-comdat-from-id comdat)
		       (if (has-implicit-comdat raw-linkage)
			   1 ; TODO : I don't understand why this should be so
			   )))
      `((:type . ,type) (:addr-space . ,addr-space) (:constant . ,constant)
	(:linkage . ,linkage) (:alignment . ,alignment)
	(:section . ,section) (:visibility . ,visibility) (:thread-local . ,thread-local)
	(:unnamed-addr . ,unnamed-addr) (:external-init . ,external-init)
	(:dll-storage . ,dll-storage) (:comdat . ,comdat)))))
      
			   
;; TODO : numbers (codes) of blocks are defined elsewhere
;; TODO : global cleanup upon exit from this routine
(define-block module (:on-undefined-blocks :error :on-repeat :error :on-undefined-records :error)
  (blocks block-info paramattr paramattr-group type value-symtab
	  constants metadata metadata-kind function uselist operand-bundle-tags)
  ;; Order of record specs is important, as this encodes their codes
  (records ((version 1) int
	    (:side-effect (cond ((equal 0 (car res)) (setf use-relative-ids nil))
				((equal 1 (car res)) (setf use-relative-ids t))
				(t (llvm-read-error "Unsupported LLVM bitcode version: ~a" (car res))))))
	   (target-triple string) ; TODO : what does module->setTargetTriple do?
	   (datalayout string)
	   (asm string)
	   (section-name string (:side-effect (push (car res) section-table)))
	   (deplib string)	   
	   (global-var #'parse-global-var)
	   (function #'parse-function)
	   (alias-old #'parse-alias-old)
	   (purge-vals int (:side-effect (setf value-list (subseq value-list 0 (car res)))))
	   (gc-name string (:side-effect (push (car res) gc-table)))
	   (comdat (selection-kind (parse-with-function
				    (lambda-enum-parser 1 (any exact-match largest no-duplicates same-size))))
		   (name (parse-rest-with-function #'lengthed-string))
		   ;; TODO : in C++ there are more side-effects. Should I implement them?
		   (:side-effect (push res comdat-list)))
	   (vst-offset int (:side-effect (setf vst-offset (car res))))
	   (alias #'parse-alias)
	   ;; TODO : + some additional consistency checks
	   (metadata-values int (:side-effect (set metadata-list (make-list (car res) nil))))
	   (source-filename string)))
		   






;; TODO : I've no clue 
;; ;; It was :SKIP for original LLVM, but I guess it's not right -- malformation will go unnoticed
;; (define-block metadata-kind (:on-undefined-blocks :skip :on-undefined-records :warn)
;;   (records (string string)
;; 	   (epoch int)))
