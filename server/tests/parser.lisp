(in-package :ale-tests)

(define-test parser
  (let ((p (make-instance 'ale::parser)))
    (with-accessors ((tree ale::tree) (predicates ale::predicates)) p
      (assert-equal T (ale::evaluate-string-formula p "0"))
      (assert-equal "0" (ale::infix (ale::tree p)))
      (assert-equal "" (ale::dnf p))
      (assert-equal "" (ale::dnf-simplified p))
      (assert-equal "" (ale::nandify p))
      
      (assert-equal T (ale::evaluate-string-formula p "1"))
      (assert-equal "1" (ale::infix (ale::tree p)))
      (assert-equal "" (ale::dnf p))
      (assert-equal "" (ale::dnf-simplified p))
      (assert-equal "" (ale::nandify p))
      
      (assert-equal T (ale::evaluate-string-formula p "A"))
      (assert-equal "A" (ale::infix (ale::tree p)))
      (assert-equal "A" (ale::dnf p))
      (assert-equal "A" (ale::dnf-simplified p))
      (assert-equal "A" (ale::nandify p))
      (assert-equal
       '((:VALS (0) :ROW (0) :RESULT NIL :CHECKED NIL)
	 (:VALS (1) :ROW (1) :RESULT T :CHECKED NIL))
       (ale::gen-truth-table p))
      
      (assert-equal T (ale::evaluate-string-formula p "~(A)"))
      (assert-equal "~A" (ale::infix (ale::tree p)))
      (assert-equal "~(A)" (ale::dnf p))
      (assert-equal "~(A)" (ale::dnf-simplified p))
      (assert-equal "%(A,A)" (ale::nandify p))
      (assert-equal
       '((:VALS (0) :ROW (0) :RESULT T :CHECKED NIL)
	(:VALS (1) :ROW (1) :RESULT NIL :CHECKED NIL))
       (ale::gen-truth-table p))
      
      (assert-equal T (ale::evaluate-string-formula p ">(A,B)"))
      (assert-equal "(A > B)" (ale::infix (ale::tree p)))
      
      (assert-equal T (ale::evaluate-string-formula p ">(A,0)"))
      (assert-equal "(A > 0)" (ale::infix (ale::tree p)))
      
      (assert-equal T (ale::evaluate-string-formula p "=(A,B)"))
      (assert-equal T (ale::evaluate-string-formula p "&(A,B)"))
      (assert-equal T (ale::evaluate-string-formula p "%(A,B)"))
      (assert-equal T (ale::evaluate-string-formula p "|(A,B)"))
      (assert-equal nil (ale::evaluate-string-formula p ">(A, B, C)"))
      (assert-equal nil (ale::evaluate-string-formula p "&(A,C)    2"))
      )))
      
