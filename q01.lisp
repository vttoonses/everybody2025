(defun move-index (index turns name-list)
  (if turns
    (let* ((turn (car turns)) (direction (char turn 0)) (distance (parse-integer (subseq turn 1))) (name-count (length name-list)))
      (cond
        ((char= direction #\L) (move-index (decf index (if (<= distance index) distance index)) (cdr turns) name-list))
        (t (move-index (incf index (if (< (+ distance index) name-count) distance (- name-count 1 index))) (cdr turns) name-list))))
    (nth index name-list)))

(defun move-parent-index (index turns name-list)
  (if turns
    (let* ((turn (car turns)) (direction (char turn 0)) (distance (parse-integer (subseq turn 1))) (name-count (length name-list)))
      (cond
        ((char= direction #\L)
          (move-parent-index (mod (decf index distance) name-count) (cdr turns) name-list))
        (t (move-parent-index (mod (incf index distance) name-count) (cdr turns) name-list))))
    (nth index name-list)))

(defun find-last-parent (index turns name-list)
  (if turns
    (let* (
      (turn (car turns))
      (direction (char turn 0))
      (distance (parse-integer (subseq turn 1)))
      (name-count (length name-list))
      (target (cond ((char= direction #\L) (mod (- index distance) name-count)) (t (mod (+ index distance) name-count)))))
        (rotatef (nth 0 name-list) (nth target name-list))
        (find-last-parent 0 (cdr turns) name-list)))
    (nth 0 name-list))

(defun find-name (filename move-func)
  (with-open-file (in filename :direction :input)
    (when in
      (let (
        (index 0)
        (name-list (tokenizer (read-line in nil) :delimiterp #'(lambda (char) (char= char #\,))))
        (turn-list (progn (read-line in) (tokenizer (read-line in nil) :delimiterp #'(lambda (char) (char= char #\,))))))
          (funcall move-func index turn-list name-list)))))
