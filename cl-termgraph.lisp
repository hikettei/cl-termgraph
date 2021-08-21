
(in-package :cl-termgraph)
(use-package :cl-ansi-text)

(deftype line-colors ()
  `(member :black
	   :red
	   :green
	   :yellow
	   :blue
	   :magenta
	   :cyan
	   :white))

(defclass simple-graph-frame ()
  ((width :accessor frame-width
	  :initarg :width
	  :initform nil)
   (height :accessor frame-height
	   :initarg :height
	   :initform nil)))

(defmethod fill-with-blank ((frame simple-graph-frame))
  (with-slots ((x width) (y height)) frame
    (loop for i from 1 to y
          do (loop for j from 1 to x
		   do (princ (white " " :style :background)))
	  do (princ #\Newline))))


