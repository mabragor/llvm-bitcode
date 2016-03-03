
(in-package :cl-user)

(defpackage :llvm-bitcode-tests
  (:use :cl :llvm-bitcode :fiveam :flexi-streams :iterate :bit-smasher)
  (:shadowing-import-from :llvm-bitcode :inext-or-error :code :fields :abbrev-id)
  (:shadowing-import-from :cl-fad :merge-pathnames-as-file)
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
      
(test test-helloworld-parse
  (is (equal '((:block :id 8 :abbr-len 3 :len 255
		       ((:unabbrev-record ((code . 1) (fields 1)))
			(:block-info :abbr-len 2 :len 18
				     ((:set-block-id 14) (:define-abbrev 4 (:fixed 3) (:vbr 8) :array (:fixed 8)) (:define-abbrev 5 (:literal 1) (:vbr 8) :array (:fixed 7))
				      (:define-abbrev 6 (:literal 1) (:vbr 8) :array :char6) (:define-abbrev 7 (:literal 2) (:vbr 8) :array :char6) (:set-block-id 11)
				      (:define-abbrev 4 (:literal 1) (:fixed 4)) (:define-abbrev 5 (:literal 4) (:vbr 8)) (:define-abbrev 6 (:literal 11) (:fixed 4) (:fixed 4) (:vbr 8))
				      (:define-abbrev 7 (:literal 2)) (:set-block-id 12) (:define-abbrev 4 (:literal 20) (:vbr 6) (:vbr 4) (:fixed 1))
				      (:define-abbrev 5 (:literal 2) (:vbr 6) (:vbr 6) (:fixed 4)) (:define-abbrev 6 (:literal 2) (:vbr 6) (:vbr 6) (:fixed 4) (:fixed 7))
				      (:define-abbrev 7 (:literal 3) (:vbr 6) (:fixed 4) (:fixed 4)) (:define-abbrev 8 (:literal 10)) (:define-abbrev 9 (:literal 10) (:vbr 6))
				      (:define-abbrev 10 (:literal 15)) (:end-block)))
			(:block :id 10 :abbr-len 3 :len 70
				((:unabbrev-record
				  ((code . 3)
				   (fields 1 4294967295 0 18 0 33 4 108 101 115 115 45 112 114 101 99 105 115 101 45 102 112 109 97 100 0 102 97 108 115 101 0 4 110 111 45
					   102 114 97 109 101 45 112 111 105 110 116 101 114 45 101 108 105 109 0 102 97 108 115 101 0 4 110 111 45 105 110 102 115 45 102 112 45 109 97 116 104 0
					   102 97 108 115 101 0 4 110 111 45 110 97 110 115 45 102 112 45 109 97 116 104 0 102 97 108 115 101 0 4 115 116 97 99 107 45 112 114 111 116 101 99 116
					   111 114 45 98 117 102 102 101 114 45 115 105 122 101 0 56 0 4 117 110 115 97 102 101 45 102 112 45 109 97 116 104 0 102 97 108 115 101 0 4 117 115 101
					   45 115 111 102 116 45 102 108 111 97 116 0 102 97 108 115 101 0)))
				 (:unabbrev-record ((code . 3) (fields 2 1 0 11 0 21)))
				 (:unabbrev-record ((code . 3) (fields 3 4294967295 0 18))) (:end-block)))
			(:block :id 9 :abbr-len 3 :len 2
				((:unabbrev-record ((code . 2) (fields 1))) (:unabbrev-record ((code . 2) (fields 2 3)))
				 (:end-block)))
			(:block :id 17 :abbr-len 4 :len 14
				((:define-abbrev 4 (:literal 8) (:fixed 4) (:literal 0)) (:define-abbrev 5 (:literal 21) (:fixed 1) :array (:fixed 4))
				 (:define-abbrev 6 (:literal 18) (:fixed 1) :array (:fixed 4)) (:define-abbrev 7 (:literal 19) :array :char6)
				 (:define-abbrev 8 (:literal 20) (:fixed 1) :array (:fixed 4)) (:define-abbrev 9 (:literal 11) (:vbr 8) (:fixed 4))
				 (:unabbrev-record ((code . 1) (fields 12))) (:unabbrev-record ((code . 7) (fields 8)))
				 (:abbrev-record ((code . 11) (abbrev-id . 9) (fields #*0001101 0))) (:abbrev-record ((code . 8) (abbrev-id . 4) (fields 1 0)))
				 (:unabbrev-record ((code . 7) (fields 32))) (:abbrev-record ((code . 21) (abbrev-id . 5) (fields 0 3)))
				 (:abbrev-record ((code . 8) (abbrev-id . 4) (fields 4 0))) (:abbrev-record ((code . 8) (abbrev-id . 4) (fields 0 0)))
				 (:abbrev-record ((code . 21) (abbrev-id . 5) (fields 0 3 6))) (:abbrev-record ((code . 8) (abbrev-id . 4) (fields 7 0)))
				 (:unabbrev-record ((code . 16) (fields))) (:unabbrev-record ((code . 7) (fields 64)))
				 (:unabbrev-record ((code . 2) (fields))) (:end-block)))
			(:unabbrev-record ((code . 2) (fields 120 56 54 95 54 52 45 117 110 107 110 111 119 110 45 108 105 110 117 120 45 103 110 117)))
			(:unabbrev-record
			 ((code . 3)
			  (fields 101 45 109 58 101 45 105 54 52 58 54 52 45 102 56 48 58 49 50 56 45 110 56 58 49 54 58 51 50 58 54 52 45 83 49 50 56)))
			(:define-abbrev 4 (:literal 7) (:fixed 2) (:fixed 1) (:vbr 6) (:fixed 4) (:literal 0) (:literal 0))
			(:unabbrev-record ((code . 7) (fields 2 1 4 9 0 0 0 0 1 0 0 0)))
			(:unabbrev-record ((code . 8) (fields 5 0 0 0 1 0 0 0 0 0 0 0 0 0)))
			(:unabbrev-record ((code . 8) (fields 8 0 1 0 2 0 0 0 0 0 0 0 0 0)))
			(:block :id 11 :abbr-len 4 :len 8
				((:define-abbrev 8 (:literal 7) :array (:fixed 3)) (:define-abbrev 9 (:literal 8) :array (:fixed 8)) (:define-abbrev 10 (:literal 9) :array (:fixed 7))
				 (:define-abbrev 11 (:literal 9) :array :char6) (:abbrev-record ((code . 1) (abbrev-id . 4) (fields 1)))
				 (:abbrev-record ((code . 9) (abbrev-id . 10) (fields 72 101 108 108 111 32 119 111 114 108 100 33))) (:end-block)))
			(:block :id 15 :abbr-len 3 :len 19
				((:define-abbrev 4 (:literal 1) :array (:fixed 8)) (:define-abbrev 5 (:literal 4) :array (:fixed 8))
				 (:abbrev-record
				  ((code . 1) (abbrev-id . 4)
				   (fields 99 108 97 110 103 32 118 101 114 115 105 111 110 32 51 46 54 46 50 32 40 98 114 97 110 99 104 101 115 47 114 101 108 101 97 115 101 95 51 54 32 50 53
					   53 54 52 50 41)))
				 (:unabbrev-record ((code . 3) (fields 1)))
				 (:abbrev-record ((code . 4) (abbrev-id . 5) (fields 108 108 118 109 46 105 100 101 110 116)))
				 (:unabbrev-record ((code . 10) (fields 1))) (:end-block)))
			(:block :id 15 :abbr-len 3 :len 50
				((:unabbrev-record ((code . 6) (fields 0 100 98 103)))
				 (:unabbrev-record ((code . 6) (fields 1 116 98 97 97)))
				 (:unabbrev-record ((code . 6) (fields 2 112 114 111 102)))
				 (:unabbrev-record ((code . 6) (fields 3 102 112 109 97 116 104)))
				 (:unabbrev-record ((code . 6) (fields 4 114 97 110 103 101)))
				 (:unabbrev-record ((code . 6) (fields 5 116 98 97 97 46 115 116 114 117 99 116)))
				 (:unabbrev-record ((code . 6) (fields 6 105 110 118 97 114 105 97 110 116 46 108 111 97 100)))
				 (:unabbrev-record ((code . 6) (fields 7 97 108 105 97 115 46 115 99 111 112 101)))
				 (:unabbrev-record ((code . 6) (fields 8 110 111 97 108 105 97 115)))
				 (:unabbrev-record ((code . 6) (fields 9 110 111 110 116 101 109 112 111 114 97 108)))
				 (:unabbrev-record
				  ((code . 6)
				   (fields 10 108 108 118 109 46 109 101 109 46 112 97 114 97 108 108 101 108 95 108 111 111 112 95 97 99 99 101 115 115)))
				 (:unabbrev-record ((code . 6) (fields 11 110 111 110 110 117 108 108))) (:end-block)))
			(:block :id 14 :abbr-len 4 :len 4
				((:abbrev-record ((code . 1) (abbrev-id . 6) (fields #*0000001 #\m #\a #\i #\n)))
				 (:abbrev-record ((code . 1) (abbrev-id . 6) (fields #*0000000 #\s #\t #\r)))
				 (:abbrev-record ((code . 1) (abbrev-id . 6) (fields #*0000010 #\p #\u #\t #\s))) (:end-block)))
			(:block :id 12 :abbr-len 4 :len 15
				((:unabbrev-record ((code . 1) (fields 1)))
				 (:block :id 11 :abbr-len 4 :len 4
					 ((:abbrev-record ((code . 1) (abbrev-id . 4) (fields 3))) (:unabbrev-record ((code . 2) (fields)))
					  (:abbrev-record ((code . 1) (abbrev-id . 4) (fields 10))) (:unabbrev-record ((code . 2) (fields)))
					  (:abbrev-record ((code . 1) (abbrev-id . 4) (fields 6))) (:unabbrev-record ((code . 20) (fields 2 0 10 5 10 5))) (:end-block)))
				 (:unabbrev-record ((code . 34) (fields 0 1 5 1))) (:abbrev-record ((code . 10) (abbrev-id . 9) (fields #*00100)))
				 (:block :id 14 :abbr-len 4 :len 3
					 ((:abbrev-record ((code . 2) (abbrev-id . 7) (fields #*0000000 #\e #\n #\t #\r #\y)))
					  (:abbrev-record ((code . 1) (abbrev-id . 6) (fields #*0000111 #\p #\u #\t #\s))) (:end-block)))
				 (:end-block)))
			(:end-block))))
	     (read-bc-file (merge-pathnames-as-file (asdf:system-source-directory :llvm-bitcode)
						    "helloworld.bc")
			   :debug t)))
  (is (equal '((:block 8
		 ((:record ((code . 1) (fields 1)))
		  (:block 10
		    ((:record
		      ((code . 3)
		       (fields 1 4294967295 0 18 0 33 4 108 101 115 115 45 112 114 101 99 105 115 101 45 102 112 109 97 100 0 102 97 108 115 101 0 4 110 111 45 102 114 97 109
			       101 45 112 111 105 110 116 101 114 45 101 108 105 109 0 102 97 108 115 101 0 4 110 111 45 105 110 102 115 45 102 112 45 109 97 116 104 0 102 97 108 115
			       101 0 4 110 111 45 110 97 110 115 45 102 112 45 109 97 116 104 0 102 97 108 115 101 0 4 115 116 97 99 107 45 112 114 111 116 101 99 116 111 114 45 98
			       117 102 102 101 114 45 115 105 122 101 0 56 0 4 117 110 115 97 102 101 45 102 112 45 109 97 116 104 0 102 97 108 115 101 0 4 117 115 101 45 115 111 102
			       116 45 102 108 111 97 116 0 102 97 108 115 101 0)))
		     (:record ((code . 3) (fields 2 1 0 11 0 21))) (:record ((code . 3) (fields 3 4294967295 0 18))) ))
		  (:block 9 ((:record ((code . 2) (fields 1))) (:record ((code . 2) (fields 2 3))) ))
		  (:block 17
		    ((:record ((code . 1) (fields 12))) (:record ((code . 7) (fields 8))) (:record ((code . 11) (abbrev-id . 9) (fields #*0001101 0)))
		     (:record ((code . 8) (abbrev-id . 4) (fields 1 0))) (:record ((code . 7) (fields 32))) (:record ((code . 21) (abbrev-id . 5) (fields 0 3)))
		     (:record ((code . 8) (abbrev-id . 4) (fields 4 0))) (:record ((code . 8) (abbrev-id . 4) (fields 0 0)))
		     (:record ((code . 21) (abbrev-id . 5) (fields 0 3 6))) (:record ((code . 8) (abbrev-id . 4) (fields 7 0))) (:record ((code . 16) (fields)))
		     (:record ((code . 7) (fields 64))) (:record ((code . 2) (fields))) ))
		  (:record ((code . 2) (fields 120 56 54 95 54 52 45 117 110 107 110 111 119 110 45 108 105 110 117 120 45 103 110 117)))
		  (:record ((code . 3) (fields 101 45 109 58 101 45 105 54 52 58 54 52 45 102 56 48 58 49 50 56 45 110 56 58 49 54 58 51 50 58 54 52 45 83 49 50 56)))
		  (:record ((code . 7) (fields 2 1 4 9 0 0 0 0 1 0 0 0))) (:record ((code . 8) (fields 5 0 0 0 1 0 0 0 0 0 0 0 0 0)))
		  (:record ((code . 8) (fields 8 0 1 0 2 0 0 0 0 0 0 0 0 0)))
		  (:block 11
		    ((:record ((code . 1) (abbrev-id . 4) (fields 1))) (:record ((code . 9) (abbrev-id . 10) (fields 72 101 108 108 111 32 119 111 114 108 100 33)))
		     ))
		  (:block 15
		    ((:record
		      ((code . 1) (abbrev-id . 4)
		       (fields 99 108 97 110 103 32 118 101 114 115 105 111 110 32 51 46 54 46 50 32 40 98 114 97 110 99 104 101 115 47 114 101 108 101 97 115 101 95 51 54 32 50 53
			       53 54 52 50 41)))
		     (:record ((code . 3) (fields 1))) (:record ((code . 4) (abbrev-id . 5) (fields 108 108 118 109 46 105 100 101 110 116)))
		     (:record ((code . 10) (fields 1))) ))
		  (:block 15
		    ((:record ((code . 6) (fields 0 100 98 103))) (:record ((code . 6) (fields 1 116 98 97 97))) (:record ((code . 6) (fields 2 112 114 111 102)))
		     (:record ((code . 6) (fields 3 102 112 109 97 116 104))) (:record ((code . 6) (fields 4 114 97 110 103 101)))
		     (:record ((code . 6) (fields 5 116 98 97 97 46 115 116 114 117 99 116)))
		     (:record ((code . 6) (fields 6 105 110 118 97 114 105 97 110 116 46 108 111 97 100)))
		     (:record ((code . 6) (fields 7 97 108 105 97 115 46 115 99 111 112 101))) (:record ((code . 6) (fields 8 110 111 97 108 105 97 115)))
		     (:record ((code . 6) (fields 9 110 111 110 116 101 109 112 111 114 97 108)))
		     (:record ((code . 6) (fields 10 108 108 118 109 46 109 101 109 46 112 97 114 97 108 108 101 108 95 108 111 111 112 95 97 99 99 101 115 115)))
		     (:record ((code . 6) (fields 11 110 111 110 110 117 108 108))) ))
		  (:block 14
		    ((:record ((code . 1) (abbrev-id . 6) (fields #*0000001 #\m #\a #\i #\n)))
		     (:record ((code . 1) (abbrev-id . 6) (fields #*0000000 #\s #\t #\r)))
		     (:record ((code . 1) (abbrev-id . 6) (fields #*0000010 #\p #\u #\t #\s))) ))
		  (:block 12
		    ((:record ((code . 1) (fields 1)))
		     (:block 11
		       ((:record ((code . 1) (abbrev-id . 4) (fields 3))) (:record ((code . 2) (fields))) (:record ((code . 1) (abbrev-id . 4) (fields 10)))
			(:record ((code . 2) (fields))) (:record ((code . 1) (abbrev-id . 4) (fields 6))) (:record ((code . 20) (fields 2 0 10 5 10 5))) ))
		     (:record ((code . 34) (fields 0 1 5 1))) (:record ((code . 10) (abbrev-id . 9) (fields #*00100)))
		     (:block 14
		       ((:record ((code . 2) (abbrev-id . 7) (fields #*0000000 #\e #\n #\t #\r #\y)))
			(:record ((code . 1) (abbrev-id . 6) (fields #*0000111 #\p #\u #\t #\s))) ))
		     ))
		  )))
	     (read-bc-file (merge-pathnames-as-file (asdf:system-source-directory :llvm-bitcode)
						    "helloworld.bc")
			   :debug nil))))
	     
