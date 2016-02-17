
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

;; (define-block module 8
;;   (subblocks block-info paramattr type type-symtab value-symtab
;; 	     constants function metadata)
;;   (records version triple data-layout asm section-name deplib
;; 	   global-var function alias purge-vals gc-name))



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
;; * paramattr
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

(define-block paramattr (:on-repeat :error :on-undefined-blocks :skip :on-undefined-records :skip)
  (records (entry-old ...) ; here is some sort-of complex parsing, modifying the context
	   (entry ...)))

(defmacro define-enum-parser (name base-index &body kwds)
  ;; TODO : this should expand into parser definition
  `(defun parse-... (x)
     )
  nil)

(define-enum-parser attribute-kind 1
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



;; TODO : I've no clue 
;; ;; It was :SKIP for original LLVM, but I guess it's not right -- malformation will go unnoticed
;; (define-block metadata-kind (:on-undefined-blocks :skip :on-undefined-records :warn)
;;   (records (string string)
;; 	   (epoch int)))
