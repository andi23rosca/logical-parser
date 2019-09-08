(in-package :ale)
(defun is-whitespace (chr)
  "Checks if a given character is a whitespace character."
  (not (eql nil
	    (find chr '(#\Space #\Newline #\Backspace #\Tab #\Linefeed)))))
(defun remove-whitespace (ls)
  "Removes all whitespace characters from a list."
  (remove-if #'is-whitespace ls))

(defclass lexer ()
  ((input
    :initform '()
    :accessor input)
   (pointer
    :initform 0
    :accessor pointer)))

(defgeneric read-input (lexer)
  (:documentation "Reads and sanitizes input."))
(defmethod read-input ((l lexer))
  (setf (pointer l) 0)
  (setf (input l) (remove-whitespace (coerce (read-line) 'list))))

(defmethod read-string-input ((l lexer) str)
  (setf (pointer l) 0)
  (setf (input l) (remove-whitespace (coerce str 'list))))

(defgeneric advance (lexer)
  (:documentation "Advances the pointer of the lexer to the next token."))
(defmethod advance ((l lexer))
  (incf (pointer l)))

(defgeneric token (lexer)
  (:documentation "Gets the current token to which the pointer is pointing to."))
(defmethod token ((l lexer))
  (nth (pointer l) (input l)))
(defmethod last-token ((l lexer))
  (nth (- (pointer l) 1) (input l)))

(defgeneric match-token (lexer &rest tokens)
  (:documentation "Checks if the current token the lexer is pointing to and a given token mathc."))
(defmethod match-token ((l lexer) &rest tokens)
  (if (not (eql nil (find (token l) tokens)))
      (progn (advance l)
	     t)
      nil))
