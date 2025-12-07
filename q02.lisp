(defun complex-+ (op1 op2)
  (mapcar #'+ op1 op2))

(defun complex-/ (op1 op2)
  (mapcar #'floor op1 op2))

(defun complex-* (op1 op2)
  (list
    (- (* (first op1) (first op2)) (* (second op1) (second op2)))
    (+ (* (first op1) (second op2)) (* (first op2) (second op1)))))

(defun cycle (iterations num a)
  (if (> iterations 0)
    (progn
      (setf num (complex-* num num))
      (setf num (complex-/ num '(10 10)))
      (setf num (complex-+ num a))
      (cycle (1- iterations) num a))
    num))
