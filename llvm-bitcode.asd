;;;; llvm-bitcode.asd

(asdf:defsystem #:llvm-bitcode
  :description "Read and write LLVM IR in bitcode format and bitcode format in general"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :depends-on (#:iterate #:cl-itertools #:fast-io #:defmacro-enhance #:bit-smasher)
  :serial t
  :components ((:file "package")
               (:file "llvm-bitcode")))

