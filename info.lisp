;; cyrus harmon post 2011-03-04

(defun get-image-dimensions (image-var env)
  (multiple-value-bind (binding-type localp declarations)
      (sb-cltl2:variable-information image-var env)
    (format t "~A~%" (list binding-type localp declarations))))

(let ((a (make-array (list 3 3) :element-type 'single-float)))
  (get-image-dimensions a))

(defmacro var-info (var &environment env)
  (list 'quote (multiple-value-list (sb-cltl2:variable-information var env))))

(defvar *foo* nil)

(var-info *foo*)

(let ((x 1s0))
  (declare (type single-float x))
  (var-info x))

(defun blub ()
 (let ((m (make-array '(3 3 3) :element-type 'single-float)))
   (declare (type (simple-array single-float 3) m))
   (var-info m)))

(blub)

(let* ((decls  (first (last (blub))))
       (type-decl (find 'type decls :key #'car)))
  (and type-decl
       (listp type-decl)
       (= (length type-decl) 4)
       (let ((size (fourth type-decl)))
	 (if (consp size) ;; (* * *) or (32 49 23)
	     (length size) 
	     ;; just the rank, like 3
	     size))))


(defun get-array-rank (a env)
  (multiple-value-bind (binding-type localp declarations)
      (sb-cltl2:variable-information a env)
    (declare (ignore binding-type localp))
    (let ((type-decl (find 'type declarations :key #'car)))
      (and type-decl
	   (listp type-decl)
	   (= (length type-decl) 4)
	   (let ((size (fourth type-decl)))
	     (if (consp size) ;; (* * *) or (32 49 23)
		 (length size) 
		 ;; just the rank, like 3
		 size))))))

(defun get-array-type (a env)
  (multiple-value-bind (binding-type localp declarations)
      (sb-cltl2:variable-information a env)
    (declare (ignore binding-type localp))
    (let ((type-decl (find 'type declarations :key #'car)))
      (and type-decl
	   (listp type-decl)
	   (= (length type-decl) 4)
	   (third type-decl)))))

(defun is-complex-type-p (type)
  (find 'complex type))

(defmacro coerce-into-ub8 (a &optional (fun 'realpart)
			   &environment env)
  (let* ((type (get-array-type a env))
	 (dims (get-array-rank a env)))
    `(let* ((u (make-array (array-dimensions ,a)
			   :element-type '(unsigned-byte 8)))
	    (u1 (sb-ext:array-storage-vector u))
	    (a1 (sb-ext:array-storage-vector ,a)))
       (declare (type (simple-array (unsigned-byte 8) ,dims) u)
		(type (simple-array (unsigned-byte 8) 1) u1)
		(type (simple-array ,type 1) a1))
       (dotimes (i (length u1))
	 (setf (aref u1 i) (floor ,(if (is-complex-type-p type)
				       `(,fun (aref a1 i))
				       `(aref a1 i)))))
       u)))

#+nil
(let ((m (make-array '(3 3 3) :element-type '(complex single-float)
		     :initial-element (complex 1s0 2s0))))
  (declare (type (simple-array (complex single-float) 3) m))
  (coerce-into-ub8 m imagpart))