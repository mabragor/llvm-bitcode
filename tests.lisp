
(in-package :cl-user)

(defpackage :llvm-bitcode-tests
  (:use :cl :llvm-bitcode :fiveam :flexi-streams :iterate :bit-smasher)
  (:shadowing-import-from :llvm-bitcode :inext-or-error)
  (:export #:run-tests))

(in-package :llvm-bitcode-tests)

(def-suite bitcode)
(in-suite bitcode)

(defun run-tests ()
  (let ((results (run 'bitcode)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(defun test-byte-reader0 (string)
  (with-input-from-sequence (stream (string-to-octets string))
    (iter (for byte in-it (byte-reader stream))
	  (collect (int<- byte)))))

(defun test-byte-reader1 (string)
  (with-input-from-sequence (stream (string-to-octets string))
    (let ((my-it (byte-reader stream)))
      (list (int<- (inext-or-error my-it))
	    (inext-or-error my-it :align-32-bits)
	    (int<- (inext-or-error my-it))
	    (int<- (inext-or-error my-it))
	    (int<- (inext-or-error my-it))
	    (int<- (inext-or-error my-it))))))

(test byte-reader
  (is (equal '(97 98 99 100 101 102 103 104) (test-byte-reader0 "abcdefgh")))
  (is (equal '(97 nil 101 102 103 104) (test-byte-reader1 "abcdefgh"))))
      
