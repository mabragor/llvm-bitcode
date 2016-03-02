
(in-package llvm-bitcode)

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

;; TODO : numbers (codes) of blocks are defined elsewhere
;; TODO : global cleanup upon exit from this routine
(define-block module (:on-undefined-blocks :error) ; :on-repeat :error :on-undefined-records :error)
  (:around ;; here we write all the context-establishing things
   (let ((use-relative-ids nil))
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
