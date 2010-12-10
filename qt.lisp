(defparameter *D* 2)
(defparameter *pmax* 16)

(defparameter *total-boxes* (floor (1- (expt 2 (+ *d* *pmax*)))
				   (1- (expt 2 *d*))))

(deftype box-index ()
  `(integer 1 #.*total-boxes*))

(defun mother (k)
  (declare (box-index k)
	   (values (integer 0 #.*total-boxes*) &optional))
  (let* ((e (expt 2 *D*))
	 (r (floor (+ k e -2)
		   e)))
    r))

(defun left-daughter (k)
  (declare (box-index k)
	   (values box-index &optional))
  (let ((e (expt 2 *D*)))
    (+ 2 (* e (1- k)))))

(defun right-daughter (k)
  (declare (box-index k)
	   (values box-index &optional))
  (let ((e (expt 2 *D*)))
    (1+ (* e k))))

#+nil
(list (mother 23)
      (left-daughter 6)
      (right-daughter 6))



(defclass vec ()
  ((data :accessor data :initarg :data :type (array single-float 1)
	 :initform (make-array *d* :element-type 'single-float
			       :initial-element 0s0))))

(defun v (&optional (y 0s0) (x 0s0))
  (make-instance 'vec :data (make-array *d* :element-type 'single-float
					:initial-contents (list y x))))

(defmethod print-object ((v vec) stream)
  (with-slots (data) v
   (format stream "#<vec~{ ~2,2f~}>" (map 'list #'identity data))))

(defmethod vref ((v vec) i)
  (declare ((integer 0 #.*d*) i)
	   (values single-float &optional))
  (aref (data v) i))

(defmethod eqv ((a vec) (b vec))
  (declare (values boolean &optional))
  (dotimes (i *d*)
    (when (< 1e-7 (abs (- (vref a i) (vref b i))))
      (return-from eqv nil)))
  t)

#+nil
(list
 (eqv (v .1 .2)
      (v .2 .1))
 (eqv (v .1 .2)
      (v .1 .2))
 (eqv (v .1 .2)
      (v .2 .2)))

(defclass su ()
  ((center :accessor center :initarg :center :type vec
	   :initform (v))
   (radius :accessor radius :initarg :radius :type single-float
	   :initform 0s0)))
#+nil
(make-instance 'su)

(defmethod print-object ((su su) stream)
  (with-slots (center radius) su
   (format stream "#<su ~f ~a>" radius center)))


(defmethod eqv ((a su) (b su))
  (and (= (radius a) (radius b))
       (eqv (center a) (center b))))

#+nil
(eqv (make-instance 'su)
     (make-instance 'su :radius .1))


(defclass box ()
  ((lo :accessor lo :initarg :lo :initform (v) :type vec)
   (hi :accessor hi :initarg :hi :initform (v 1s0 1s0) :type vec)))

(defmethod inboxp ((su su) (box box))
  "Check whether a sphere is contained within the box."
  (declare (values boolean &optional))
  (with-slots (center radius) su
   (dotimes (i *d*)
     (when (or (< (- (vref center i) radius)
		  (vref (lo box) i)) 
	       (< (vref (hi box) i)
		  (+ (vref center i) radius)))
       (return-from inboxp nil))))
  t)

#+nil
(inboxp (make-instance 'su :center (v .5 .5) :radius .1s0)
	(make-instance 'box))


(defmethod dist ((a vec) (b vec))
  "Distance between two vectors."
  (declare (values single-float &optional))
  (let ((sum 0s0))
    (dotimes (i *d*)
      (incf sum (let ((q (- (vref a i) (vref b i))))
		  (* q q))))
    (sqrt sum)))

#+nil
(dist (v) (v .1))

(defmethod contains ((point vec) (su su))
  "Check if a point is inside sphere."
  (declare (values boolean &optional))
  (with-slots (center radius) su
    (unless (< radius (dist point center))
      t))) 

#+nil
(contains (v 1s0) (make-instance 'su :radius .999s0))

(defmethod contains ((a su) (b su))
  "Check if two spheres collide."
  (declare (values boolean &optional))
  (when (< (dist (center a)
		 (center b))
	   (+ (radius a)
	      (radius b)))
    t))


(defclass qtree ()
  ((maxdepth :accessor maxdepth :initarg :maxdepth :initform *pmax* :type fixnum)
   (blo :accessor blo :initarg :blo :initform (v) :type vec)
   (bscale :accessor bscale :initarg :bscale :initform (v 1s0 1s0) :type vec)
   (elhash :accessor elhash :initarg :elhash 
	   :initform (make-hash-table :size nh) :type hash-table)
   (pophash :accessor pophash :initarg :elhash 
	    :initform (make-hash-table :size ) :type)))
