(in-package :ale)

(defun push-to-list (item ls)
  "Appends item to end of list"
  (reverse (cons item (reverse ls))))

(defun implication (n m)
  (or m (not n)))
(defun nand (n m)
  (not (and n m)))


(defmacro with-initial (value &rest body)
  "Wrap this macro around code and use :revert-to-initial to revert a variable to the value it had before the wrapped code mutated it."
  (let ((initial-value (gensym)))
    (let ((new-body (subst `(setf ,value ,initial-value) :revert-to-initial body)))
    `(let ((,initial-value ,value))
       ,@new-body))))

(defvar predicates-list '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(defun join (list &optional (delimiter #\&))
  (with-output-to-string (stream)
    (join-to-stream stream list delimiter)))

(defun join-to-stream (stream list &optional (delimiter #\&))
  (destructuring-bind (&optional first &rest rest) list
    (when first
      (write-string first stream)
      (when rest
        (write-char delimiter stream)
        (join-to-stream stream rest delimiter)))))

(defmethod wrap-name-stars (name)
  (intern (concatenate 'string "*" (string name) "*")))

(defun remove-stars (str)
  (subseq (string str) 1 2))

(defun binlist->int (ls)
  (loop for val in (reverse ls)
     for pow = 0 then (1+ pow)
     sum (if (zerop val)
	     0
	     (expt 2 pow))))
