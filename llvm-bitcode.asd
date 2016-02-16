;;;; llvm-bitcode.asd

(defpackage :llvm-bitcode-system
  (:use :cl :asdf))

(in-package llvm-bitcode-system)

(defsystem #:llvm-bitcode
  :description "Read and write LLVM IR in bitcode format and bitcode format in general"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:iterate #:cl-itertools #:fast-io #:defmacro-enhance
			 #:bit-smasher #:quasiquote-2.0 #:cl-interpol)
  :serial t
  :components ((:file "package")
               (:file "generic-bitcode-reader")
	       ;; (:file "llvm-bitcode-reader")
	       ))

(defsystem #:llvm-bitcode-tests
  :description "Tests for LLVM-BITCODE."
  :licence "MIT"
  :depends-on (:llvm-bitcode :fiveam :flexi-streams :iterate :bit-smasher :cl-fad)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :llvm-bitcode))))
  (load-system :llvm-bitcode-tests)
  (funcall (intern "RUN-TESTS" :llvm-bitcode-tests)))
