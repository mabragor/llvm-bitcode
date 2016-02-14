
;;;; llvm-bitcode-reader.lisp

(in-package #:llvm-bitcode)

(quasiquote-2.0:enable-quasiquote-2.0)
(cl-interpol:enable-interpol-syntax)

;; TODO : somehow I should restrict only to LLVM IR namespace

;; OK, let's first just copy all the enums we have in the LLVMBitCodes.h
;; and then see, how to write it in a more convenient form.

;; TODO : this one should start from FIRST-APPLICATION-BLOCK-ID
(defparameter block-ids '(module paramattr paramattr-group constants function
			  unused-id1 value-symtab metadata metadata-attachment
			  type use-list))

;; TODO : these all are 1-based
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
