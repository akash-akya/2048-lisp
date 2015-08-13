(defparameter *size* 5)
(setf *arr* (make-array (list *size* *size*) :initial-element 0))

(defun get-ele (row col)
    (aref *arr* row col))

(defun set-ele (row col element)
    (setf (aref *arr* row col) element))

(defun get-row (row &optional (index 0))
    (if (= index *size*) nil
        (cons (get-ele row index) (get-row row (1+ index)))))

(defun get-col (col &optional (index 0))
    (if (= index *size*) nil
        (cons (get-ele index col) (get-col col (1+ index)))))

(defun set-col (lst col)
    (dotimes (i *size*)
        (set-ele i col (pop lst))))

(defun set-row (lst row)
    (dotimes (i *size*)
        (set-ele row i (pop lst))))

(defun merge-list (lst)
    (if (null (second lst))
        lst
        (if (= (first lst) (second lst))
            (cons (+ (pop lst) (pop lst)) (merge-list lst))
            (cons (pop lst) (merge-list lst)))))

(defun sufix-zero (x)
  (append x (make-list (- *size* (length x)) :initial-element 0)))

(defun prefix-zero (x)
  (append (make-list (- *size* (length x)) :initial-element 0) x))

(defun move-up ()
    (dotimes (i *size*)
      (set-col (sufix-zero (merge-list (remove-if #'zerop (get-col i)))) i))
    (disp))

(defun move-down ()
    (dotimes (i *size*)
      (set-col (prefix-zero (merge-list (remove-if #'zerop (get-col i)))) i))
    (disp))

(defun move-left ()
    (dotimes (i *size*)
      (set-row (sufix-zero (merge-list (remove-if #'zerop (get-row i)))) i))
    (disp))
  
(defun move-right ()
    (dotimes (i *size*)
      (set-row (prefix-zero (merge-list (remove-if #'zerop (get-row i)))) i))
    (disp))

(defun disp ()
  (dotimes (i *size*)
    (format t "~s~%" (get-row i))))

(defun set-random ()
  (set-ele 0 1 2)
  (set-ele 3 3 4)
  (set-ele 4 1 2)
  (set-ele 2 3 8)
  (set-ele 2 4 2)
  (set-ele 2 0 2)
  (set-ele 1 2 4))