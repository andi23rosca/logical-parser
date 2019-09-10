(in-package :ale-tests)


(define-test implication
  (assert-equal t (ale::implication nil nil))
  (assert-equal t (ale::implication nil t))
  (assert-equal nil (ale::implication t nil))
  (assert-equal t (ale::implication t t)))

(define-test nand
  (assert-equal t (ale::nand nil nil))
  (assert-equal t (ale::nand nil t))
  (assert-equal t (ale::nand t nil))
  (assert-equal nil (ale::nand t t)))

(define-test binlist->int
  (assert-equal 1 (ale::binlist->int '(1)))
  (assert-equal 2 (ale::binlist->int '(1 0)))
  (assert-equal 7 (ale::binlist->int '(1 1 1))))
