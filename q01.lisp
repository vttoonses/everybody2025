(defun move-index (index turns name-count)
  (if turns
    (let* ((turn (car turns)) (direction (char turn 0)) (distance (parse-integer (subseq turn 1))))
      (cond
        ((char= direction #\L) (move-index (decf index (if (<= distance index) distance index)) (cdr turns) name-count))
        (t (move-index (incf index (if (< (+ distance index) name-count) distance (- name-count 1 index))) (cdr turns) name-count))))
    index))

(defun find-my-name (filename)
  (with-open-file (in filename :direction :input)
    (when in
      (let (
        (index 0)
        (name-list (tokenizer (read-line in nil) :delimiterp #'(lambda (char) (char= char #\,))))
        (turn-list (progn (read-line in) (tokenizer (read-line in nil) :delimiterp #'(lambda (char) (char= char #\,))))))
          (nth (move-index index turn-list (length name-list)) name-list)))))
