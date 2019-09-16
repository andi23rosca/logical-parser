(in-package :ale)

(start (make-instance 'easy-acceptor :port 8080))

(defun alist-add (ls items)
  (concatenate 'list ls items))
(defun bool->int (n)
  (if n 1 0))


(define-easy-handler (api :uri "/") ()
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (setf (hunchentoot:content-type*) "application/json")
  (let ((p (make-instance 'parser))
	(param (get-parameter "input"))
	(resp '())
	(tr '())
	(is-valid '()))
    (setf is-valid (evaluate-string-formula p param))
    (setf tr (gen-truth-table p))
    (setf resp
	  (alist-add
	   resp
	   (list
	    :is-valid is-valid
	    :infix (infix (tree p))
	    :ast (ast (tree p))
	    :predicates
	    (loop for pr in (predicates p)
	       collect (remove-stars pr))
	    :truth-table
	    (loop for row in tr
	       collect (push-to-list (bool->int (getf row :result))
				     (getf row :vals)))
	    :simplified-truth-table
	    (loop for row in (simplify-table tr)
	       collect (push-to-list (bool->int (getf row :result))
				     (getf row :vals)))
	    :hex
	    (hex-result tr)
	    :dnf (dnf p)
	    :simplified-dnf (dnf-simplified p)
	    :nandified (nandify p)
	    )))
    
    
    (encode-json-plist-to-string resp)))
    
    
    
