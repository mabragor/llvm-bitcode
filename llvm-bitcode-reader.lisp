
;;;; llvm-bitcode-reader.lisp

(in-package #:llvm-bitcode)

(quasiquote-2.0:enable-quasiquote-2.0)
(cl-interpol:enable-interpol-syntax)


;; TODO : somehow I should restrict only to LLVM IR thingy

(define-block module 8
  (subblocks block-info paramattr type type-symtab value-symtab
	     constants function metadata)
  (records version triple data-layout asm section-name deplib
	   global-var function alias purge-vals gc-name))



(defun string<- (lst)
  (coerce (mapcar #'char-code lst) 'string))

(define-record version 1)
(define-record triple 2 string)
(define-record data-layout 3 string)
(define-record asm 4
  (cl-ppcre:split "\\n" (string<- x)))
;; TODO : exactly one section-name per something???
(define-record section-name 5 string)
(define-record deplib 6 string) ; I've no clue what this is...

(define-record global-var 7
  ;; complicated definition
  ...)

(define-record function 8
  ;; compicated definition
  ...)

(define-record alias 9
  ;; complicated definition
  ...)

;; TODO : after this level of parsing, there is another one, which
;;      : actually uses these PURGE-VALS (and other context-altering things)
(define-record purge-vals 10)
(define-record gc-name 11 string)


(define-block param-attr 9
  (records entry))
