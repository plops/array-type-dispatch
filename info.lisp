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
 (let ((m (make-array 3 :element-type 'single-float)))
   (declare (type (simple-array single-float 1) m))
   (var-info m)))

(blub)