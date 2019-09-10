(in-package :ale-tests)

(define-test is-whitespace
  (assert-equal nil (ale::is-whitespace #\b))
  (assert-equal t (ale::is-whitespace (char " " 0))))

(define-test remove-whitespace
  (assert-equal '(#\a #\b) (ale::remove-whitespace (coerce "a    b" 'list))))


(define-test lexer
  (let ((l (make-instance 'ale::lexer)))
    (with-accessors ((input ale::input) (pointer ale::pointer)) l
      (ale::read-string-input l "> (A,  B)")
      (assert-equal '(#\> #\( #\A #\, #\B #\)) input)
      (assert-equal 0 pointer)
      (ale::advance l)
      (assert-equal 1 pointer)
      (assert-equal #\( (ale::token l))
      (assert-equal #\> (ale::last-token l))
      (assert-equal t (ale::match-token l #\) #\())
      (assert-equal nil (ale::match-token l #\,))
      (assert-equal 2 pointer))))
