(in-package :ale)
(defclass parser (lexer)
  ((tree
    :initform (make-instance 'formula-tree)
    :accessor tree)
   (predicates
    :initform '()
    :accessor predicates)))

(defmethod formula ((p parser))
  (with-accessors ((pointer pointer)) p
    (with-initial pointer
	(if (or (constant p)
		(predicate p)
		(unary-formula p)
		(binary-formula p))
	    t
	    (progn :revert-to-initial
		   nil)))))
(defmethod unary-formula ((p parser))
  (with-accessors ((pointer pointer)) p
    (with-initial pointer 
      (cond ((not (unary-operator p)) (progn :revert-to-initial nil))
	    ((not (left-paren p)) (progn :revert-to-initial nil))
	    ((not (formula p)) (progn :revert-to-initial nil))
	    ((not (right-paren p)) (progn :revert-to-initial nil))
	    (t t)))))

(defmethod binary-formula ((p parser))
  (with-accessors ((pointer pointer)) p
    (with-initial pointer
      (cond ((not (binary-operator p)) (progn :revert-to-initial nil))
	    ((not (left-paren p)) (progn :revert-to-initial nil))
	    ((not (formula p)) (progn :revert-to-initial nil))
	    ((not (comma p)) (progn :revert-to-initial nil))
	    ((not (formula p)) (progn :revert-to-initial nil))
	    ((not (right-paren p)) (progn :revert-to-initial nil))
	    (t t)))))
  
(defmethod constant ((p parser))
  (if (match-token p #\1 #\0)
      (progn
	(add-value (tree p) (last-token p))
	t)
      nil))

(defmethod predicate ((p parser))
  (if (apply #'match-token (cons p predicates-list))
      (let ((starred-val (wrap-name-stars (last-token p))))
      (progn (add-value (tree p) starred-val)
	     (pushnew starred-val (predicates p))
	     t))
      nil))
(defmethod unary-operator ((p parser))
  (if (match-token p #\~)
      (progn (add-operator (tree p) (last-token p))
	     t)
      nil))
(defmethod binary-operator ((p parser))
  (if (match-token p #\> #\= #\| #\& #\%)
      (progn (add-operator (tree p) (last-token p))
	     t)
      nil))
(defmethod left-paren ((p parser))
  (if (match-token p #\()
      (progn (nest (tree p))
	     t)
      nil))
(defmethod right-paren ((p parser))
  (if (match-token p #\))
      (progn (unnest (tree p))
	     t)
      nil))
(defmethod comma ((p parser))
  (match-token p #\,))

(defmethod sort-symbols-alpha (symbols)
  (sort symbols #'string-lessp))

(defmethod evaluate-formula ((p parser))
  (with-accessors ((pointer pointer) (input input) (tree tree) (predicates predicates)) p
    (setf tree (make-instance 'formula-tree))
    (setf predicates '())
    (read-input p)
    (if (formula p)
	(if (= pointer (length input))
	    (progn (post-process tree)
		   (setf predicates (sort-symbols-alpha predicates))
		   t)
	    (progn :revert-to-initial nil))
	nil)))

(defmethod evaluate-string-formula ((p parser) str)
  (with-accessors ((pointer pointer) (input input) (tree tree) (predicates predicates)) p
    (setf tree (make-instance 'formula-tree))
    (setf predicates '())
    (read-string-input p str)
    (if (formula p)
	(if (= pointer (length input))
	    (progn (post-process tree)
		   (setf predicates (sort-symbols-alpha predicates))
		   t)
	    (progn :revert-to-initial nil))
	nil)))

(defun int2bin2list (nr &optional (ls nil))
  (let ((quotient (floor (/ nr 2))) (remainder (mod nr 2)))
    (cond ((zerop quotient) (cons remainder ls))
	  (t (int2bin2list quotient (cons remainder ls))))))

(defun fill-left (ls number value)
  (cond ((>= (length ls) number) ls)
	(t (let ((filled ls) (left (- number (length ls))))
	     (loop for i from 1 upto left
		do (setf filled (cons value filled)))
	     filled))))
(defun fill-right (ls number value)
  (reverse (fill-left (reverse ls) number value)))

(defun generate-combinations (ln)
  (let ((combinations '()))
  (loop for i from (1- (expt 2 ln)) downto 0
     do (setf combinations (cons (fill-left (int2bin2list i) ln 0) combinations)))
  combinations))

(defmethod generate-predicate-values ((p parser))
  (with-accessors ((predicates predicates)) p
      (let ((ln (length predicates)) (pred-vals '()) (combos (generate-combinations (length predicates))))
	(loop for i from 0 upto (1- (expt 2 ln))
	   do (let ((pairs '()) (vals (nth i combos)))
		(loop for j from 0 upto (1- ln)
		   do (setf pairs (push-to-list (list (nth j predicates) (nth j vals)) pairs)))
		(setf pred-vals (push-to-list pairs pred-vals))))
	pred-vals)))


(defun extract-predicate-values (pred-vals)
  (loop for (predicate value) in pred-vals
       collect value))

(defmethod gen-truth-table ((p parser))
  (let ((predicate-values (generate-predicate-values p)) (table '()))
    (loop for pred-val in predicate-values
	 for x = 0 then (1+ x)
       do (setf
	   table
	   (push-to-list
	    (list
	     :vals (extract-predicate-values pred-val)
	     :row (list x)
	     :result (evaluate-with-predicates (tree p) pred-val)
	     :checked nil)
	    table)))
    table))


(defun parse-dnf-predicates (predicates)
  (let ((len (length predicates)))
    (cond ((zerop len) "")
	  ((= 1 len) (car predicates))
	  ((= 2 len) (concatenate 'string
				     "&(" (car predicates) ","
				     (cadr predicates) ")"))
	  (t (concatenate 'string
			  "&("
			  (parse-dnf-predicates (reverse (cdr (reverse predicates))))
			  "," (car (reverse predicates)) ")")))))
(defun parse-dnf-minterms (minterms)
  (let ((len (length minterms)))
    (cond ((zerop len) "")
	  ((= 1 len) (parse-dnf-predicates (car minterms)))
	  ((= 2 len) (concatenate 'string
				  "|("
				  (parse-dnf-predicates (car minterms)) ","
				  (parse-dnf-predicates (cadr minterms)) ")"))
	  (t (concatenate 'string
			  "|("
			  (parse-dnf-minterms (reverse (cdr (reverse minterms))))
			  "," (parse-dnf-predicates (car (reverse minterms))) ")")))))

      
    
(defmethod dnf ((p parser))
  (let ((minterms (remove-if-not (lambda (row) (getf row :result)) (gen-truth-table p))))
    (let ((dnf-predicates
	   (loop for minterm in minterms
	      collect (loop for val in (getf minterm :vals)
			 for pred in (predicates p)
			 collect (if (zerop val)
				     (concatenate 'string "~(" (remove-stars pred) ")")
				     (remove-stars pred))))))
      (parse-dnf-minterms dnf-predicates))))


(defmethod dnf-simplified ((p parser))
  (let ((minterms (remove-if-not (lambda (row)(getf row :result)) (simplify-table (gen-truth-table p)))))
    (let ((dnf-predicates
	   (loop for minterm in minterms
	      collect (remove "remove" (loop for val in (getf minterm :vals)
			 for pred in (predicates p)
			 collect (if (zerop val)
				     (concatenate 'string "~(" (remove-stars pred) ")")
				     (if (= 2 val)
					 "remove"
				     (remove-stars pred))))))))
      (parse-dnf-minterms dnf-predicates))))


(defun minterm-group (minterm)
  (loop for i in (getf minterm :vals)
     counting (= 1 i) into group
     finally (return group)))

(defun match-rows (row1 row2)
  (let ((differences 0) (matched '()))
    (loop for r1 in (getf row1 :vals)
       for r2 in (getf row2 :vals)
       do (if (= r1 r2)
	      (setf matched (push-to-list r1 matched))
	      (progn (setf matched (push-to-list 2 matched))
		     (incf differences))))
    (if (> differences 1)
	nil
	(list :vals matched
	      :row (sort
		    (concatenate 'list (getf row1 :row) (getf row2 :row))
		    #'<)
	      :result T
	      :checked nil))))

(defun simplify-table (table)
  (let ((minterms (remove-if-not (lambda (row) (getf row :result)) table))
	(prime-implicants '())
	(step-number -1)
	(steps (list (loop for i upto (length (getf (car table) :vals)) collect '()))))
    (loop for minterm in minterms
       do (let ((group (minterm-group minterm)))
	    (setf (nth group (car steps))
		  (push-to-list minterm (nth group (car steps))))))
    (loop while (and (nth (1+ step-number) steps)
		     (> (length (nth (1+ step-number) steps)) 1))
       do (progn
	    (incf step-number)
	    (let ((step (nth step-number steps)))
	      (loop for group in step
		 for group-index = 0 then (1+ group-index)
		 do
		   (let ((next-group (nth (1+ group-index) step)))
		     (if (and next-group group)
			 (loop for row in group
			    do (loop for upper-row in next-group
				  do (let ((matched (match-rows row upper-row)))
				       (if matched
					   (progn
					     (setf steps (fill-right steps (+ 2 step-number) '()))
					     (setf (nth (1+ step-number) steps)
						   (fill-right (nth (1+ step-number) steps)
							       (+ 2 group-index)
							       '()))
					     (setf (nth group-index (nth (1+ step-number) steps))
						   (pushnew matched (nth group-index (nth (1+ step-number) steps)) :test (lambda (a b) (tree-equal (getf a :vals) (getf b :vals)))))
					     (setf (getf row :checked) T)
					     (setf (getf upper-row :checked) T))))))))))))
    (loop for step in steps
       do (loop for group in step
	     do (loop for minterm in group
		   do (when (not (getf minterm :checked))
			  (setf prime-implicants
				(push-to-list minterm prime-implicants))))))
    (loop for minterm in (reverse (remove-if (lambda (row) (getf row :result)) table))
       do (setf prime-implicants
		(cons minterm prime-implicants)))
    prime-implicants))
    

(defun hex-result (table)
  (let ((results (loop for row in table
		    collect (if (getf row :result)
				1
				0))))
    (format nil "~X" (binlist->int (reverse results)))))


					; >(A,B) -> %(A,%(B,B))
					; =(A,B) ->  %(%(%(A,A),%(B,B)),%(A,B))
					; |(A,B) -> %(%(A,A),%(B,B))
					; ~(A) -> %(A,A)
					; &(A,B) -> %(%(A,B),%(A,B))

(defun parse-nand (ast)
  (if (listp ast)
      (let ((current (car ast)) (first (cadr ast)) (second (caddr ast)))
	(cond
	  ((eql current 'implication)
	   `(nand ,(parse-nand first)
		  (nand
		   ,(parse-nand second)
		   ,(parse-nand second))))
	  ((eql current 'eql)
	   `(nand (nand (nand ,(parse-nand first)
			      ,(parse-nand first))
			(nand ,(parse-nand second)
			      ,(parse-nand second)))
		  (nand ,(parse-nand first)
			,(parse-nand second))))
	  ((eql current 'or)
	   `(nand (nand ,(parse-nand first)
			,(parse-nand first))
		  (nand ,(parse-nand second)
			,(parse-nand second))))
	  ((eql current 'and)
	   `(nand (nand ,(parse-nand first)
			,(parse-nand second))
		  (nand ,(parse-nand first)
			,(parse-nand second))))
	  ((eql current 'not)
	   `(nand ,(parse-nand first)
		  ,(parse-nand first)))
	  ((eql current 'nand)
	   `(nand ,(parse-nand first)
		  ,(parse-nand second)))))
      ast))

(defun prefix-nand (ast)
  (if (listp ast)
      (concatenate 'string
		   "%("
		   (prefix-nand (cadr ast))
		   ","
		   (prefix-nand (caddr ast))
		   ")")
      (let ((token (string ast)))
	(if (= 1 (length token))
	    (write-to-string (bool->int ast))
	    (remove-stars token)))))

(defmethod nandify ((p parser))
  (prefix-nand (parse-nand (ast (tree p)))))
