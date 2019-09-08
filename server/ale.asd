(push :hunchentoot-no-ssl *features*)
(asdf:defsystem "ale"
  :description "Logical formula parser and evaluator"
  :version "0.0.1"
  :serial T
  :depends-on ("cl-json" "hunchentoot")
  :components ((:file "packages")
	       (:file "globals")
	       (:file "lexer")
	       (:file "formula-tree")
	       (:file "parser")
	       (:file "server")))
