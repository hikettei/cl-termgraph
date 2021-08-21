
(in-package :cl-termgraph)


(defparameter *dif* 0.5)

(defclass figure-graph-frame (simple-graph-frame)
  ((figure :accessor figure
	   :initarg :figure
	   :initform nil)
   (from :accessor figurefrom
	 :initarg :from
	 :initform nil)
   (end :accessor figureend
	:initarg :end
	:initform nil)))

(defmethod render-figure (frame)
  (with-slots ((figure figure) (s from) (e end)) frame
    (loop for i from s to e by *dif*
	  collect `(,i . ,(funcall figure i)))))

