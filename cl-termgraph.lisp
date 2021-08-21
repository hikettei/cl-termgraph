
(in-package :cl-termgraph)



(defclass simple-graph-frame ()
  ((width :accessor frame-width
	  :initarg :width
	  :initform nil)
   (height :accessor frame-height
	    :initarg :height
	    :initform nil)))


(defmethod plot ((frame simple-graph-frame))

  )
