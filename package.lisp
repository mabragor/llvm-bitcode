;;;; package.lisp

(defpackage #:llvm-bitcode
  (:use #:cl #:iterate #:cl-itertools #:bit-smasher #:defmacro-enhance)
  (:shadow #:bits<-)
  (:export #:byte-reader #:bit-reader #:read-bc-file))



