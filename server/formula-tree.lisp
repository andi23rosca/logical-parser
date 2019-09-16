(in-package :ale)
(defun make-tree (data)
  "Creates a new node that contains 'data' as its data."
  (cons (cons data nil) nil))
(defun first-child (tree)
  "Returns a reference to the first child of the node passed in,
  or nil if this node does not have children."
  (cdr (car tree)))
(defun next-sibling (tree)
  "Returns a reference to the next sibling of the node passed in,
  or nil if this node does not have any siblings."
  (cdr tree))
(defun current-node (tree)
  "Returns the information contained in this node."
  (car (car tree)))
(defun add-child (tree child)
  "Takes two nodes created with 'make-tree' and adds the
  second node as a child of the first. Returns the first node,
  which will be modified."
  (nconc (car tree) child)
  tree)

(defclass formula-tree ()
  ((ast
    :initform (make-tree nil) 
    :accessor ast)
   (ref-queue
    :initform '()
    :accessor ref-queue)
   (nesting
    :initform 0
    :accessor nesting)))
(defmethod initialize-instance :after ((tree formula-tree) &key)
  (next tree (ast tree)))

(defmethod nest ((tree formula-tree))
  (incf (nesting tree)))
(defmethod unnest ((tree formula-tree))
  (decf (nesting tree))
  (setf (ref-queue tree) (cdr (ref-queue tree))))

(defmethod current-ref ((tree formula-tree))
  (car (ref-queue tree)))

(defmethod next ((tree formula-tree) ref)
  (setf (ref-queue tree) (cons ref (ref-queue tree))))

(defmethod add-value ((tree formula-tree) token)
  (let ((node (cons token nil)))
    (add-child (current-ref tree) node)))

(defmethod add-operator ((tree formula-tree) token)
  (let ((node (make-tree token)))
    (add-child (current-ref tree) node)
    (next tree node)))
(defmethod post-process ((tree formula-tree))
  (let ((processed (ast tree)))
    (setf processed (cadar processed))
    (setf processed (subst t #\1 processed))
    (setf processed (subst nil #\0 processed))
    (setf processed (subst 'implication #\> processed))
    (setf processed (subst 'not #\~ processed))
    (setf processed (subst 'and #\& processed))
    (setf processed (subst 'eql #\= processed))
    (setf processed (subst 'or #\| processed))
    (setf processed (subst 'nand #\% processed))
    (setf (ast tree) processed)))
(defun is-bool (n)
  (if (listp n)
      (if n nil t)
      (string= "T" (string n))))

(defun parse-infix (ast)
  (let ((current (car ast)))
    (cond 
      ((not-empty-listp current)(parse-infix current))
      ((is-bool current) (if current "1" "0"))
      (t (let ((current (string current)))
	   (cond ((or (string= current ">")
		      (string= current "=")
		      (string= current "&")
		      (string= current "%")
		      (string= current "|"))
		  (concatenate 'string
			       "(" (parse-infix (cdr ast))
			       " " current " "
			       (parse-infix (cddr ast)) ")"))
		 ((string= "~" current)
		  (concatenate 'string current (parse-infix (cdr ast))))
		 (t (remove-stars current))))))))


(defmethod infix ((tree formula-tree))
  (let ((infixed (ast tree)))
    (if (not-empty-listp infixed)
	(progn
	  (setf infixed (subst  ">" 'implication infixed))
	  (setf infixed (subst  "&" 'and infixed))
	  (setf infixed (subst "|" 'or infixed))
	  (setf infixed (subst "=" 'eql infixed))
	  (setf infixed (subst "%" 'nand infixed))
	  (setf infixed (subst "~" 'not infixed))
	  (parse-infix infixed))
	
	(if (is-bool infixed)
	    (write-to-string (bool->int infixed))
	    (remove-stars (string infixed))))))
    

(defmethod evaluate-with-predicates ((tree formula-tree) pred-var-list)
  (let ((concrete-tree (ast tree)) (predicates pred-var-list))
    (setf predicates (subst T 1 predicates))
    (setf predicates (subst Nil 0 predicates))
    (loop for (pred val) in predicates
       do (setf concrete-tree (subst val pred concrete-tree)))
    (eval concrete-tree)))

				      
