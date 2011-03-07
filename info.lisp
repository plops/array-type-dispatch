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
    (unless type
      (error "couldn't obtain type of ~a, did you declare it?" a))
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

;; debugging this macro only seems to work with global variables
(defvar *m* (make-array '(3 3 3) :element-type '(complex single-float)
		     :initial-element (complex 1s0 2s0)))
(declaim (type (simple-array (complex single-float) 3) *m*))
(coerce-into-ub8 *m* phase)

#+nil
(let ((m (make-array '(3 3 3) :element-type '(complex single-float)
		     :initial-element (complex 1s0 2s0))))
  (declare (type (simple-array (complex single-float) 3) m))
  (coerce-into-ub8 m imagpart))

(defparameter *short-type-names*
  '((u8 . (unsigned-byte 8))
    (u16 . (unsigned-byte 16))
    (u24 . (unsigned-byte 24))
    (u32 . (unsigned-byte 32))
    (u64 . (unsigned-byte 64))
    (s8 . (signed-byte 8))
    (s16 . (signed-byte 16))
    (s24 . (signed-byte 24))
    (s32 . (signed-byte 32))
    (s64 . (signed-byte 64))
    (sf . single-float)
    (df . double-float)
    (csf . (complex single-float))
    (cdf . (complex double-float))))

(defun long-type-name (short)
  "Call with 'u8 or 'sf as parameter and get the long name."
  (second (assoc short *short-type-names*)))

#+nil
(long-type-name 'u8)

(defun short-type-name (long)
  "Call with '(unsigned-byte 8) and get the short name."
 (first 
  (rassoc long *short-type-names* :test #'equal)))

#+nil
(short-type-name '(unsigned-byte 8))

;; this matrix describes what functions will do the per-pixel conversion
;; when converting one array (rows) into another type (cols)
;;      from
;; to    s8     u8    u32    sf      df       csf
;; s8     i   clamp  clamp  floor  floor   realpart/floor/clamp
;; u8   clamp   i    clamp  floor  floor   realpart/floor/clamp
;; u32  clamp   up     i    floor  floor   realpart/floor/clamp
;; sf    mul   mul    mul     i    coerce  realpart
;; df    mul   mul    mul    mul     i     realpart/mul
;; csf   mul   mul    mul   cmplx  cmplx       i

;; when range is lost (e.g. u16->s8), use clamp
;; when input is real and precision is lost (df->sf) use coerce
;; when real into fixnum (sf->u8), use floor
;; when fixnum into real (u8->sf), multiply with unity (1s0)
;; when lower precision real into higher precsion, mul with unity (1d0)
;; when converting from real into complex call (complex .. 0s0)]

(defmacro convert (target a &optional (fun 'realpart)
		   &environment env)
  (let* ((long-target (long-type-name target))
	 (type (get-array-type a env))
	 (dims (get-array-rank a env)))
    (unless long-target
      (error "Can't find long type name for ~a." target))
    (unless type
      (error "Couldn't obtain type of ~a, did you declare it?" a))
    `(let* ((u (make-array (array-dimensions ,a)
			   :element-type ',long-target))
	    (u1 (sb-ext:array-storage-vector u))
	    (a1 (sb-ext:array-storage-vector ,a)))
       (declare (type (simple-array ,long-target ,dims) u)
		(type (simple-array ,long-target 1) u1)
		(type (simple-array ,type 1) a1))
       (dotimes (i (length u1))
	 (setf (aref u1 i) (floor ,(if (is-complex-type-p type)
				       `(,fun (aref a1 i))
				       `(aref a1 i)))))
       u)))
