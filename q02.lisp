(defun complex-+ (op1 op2)
  (mapcar #'+ op1 op2))

(defun complex-/ (op1 op2)
  (mapcar #'truncate op1 op2))

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

(defun engrave-point-p (a)
  (let ((res '(0 0)))
    (loop for lcv from 0 below 100 do
      (setf res (complex-* res res))
      (setf res (complex-/ res '(100000 100000)))
      (setf res (complex-+ res a))
      (if (or (< (first res) -1000000) (< (second res) -1000000) (> (first res) 1000000) (> (second res) 1000000))
        (return-from engrave-point-p nil)))
    t))

(defun check-pattern (start-point iterations step-size)
  (let ((point-count 0) (x (first start-point)) (y (second start-point)))
    (loop for row from y to (+ y iterations) by step-size do
      (loop for col from x to (+ x iterations) by step-size do
        (if (engrave-point-p (list col row))
          (incf point-count))))
    point-count))
