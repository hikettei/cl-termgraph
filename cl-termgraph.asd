
(in-package #:cl-user)

(asdf:defsystem :cl-termgraph
  :name "cl-termgraph"
  :description "A graphic library that plots graph for Common Lisp"
  :author "hikettei"
  :license "MIT"
  :serial t
  :components ((:file "package")
	       (:file "cl-termgraph")
	       (:file "figure")))

