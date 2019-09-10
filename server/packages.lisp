(defpackage :ale
  (:use :cl :hunchentoot :cl-json))

(defpackage :ale-tests
  (:use :cl :hunchentoot :cl-json :lisp-unit :ale))
