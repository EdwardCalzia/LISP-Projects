(defun print-board (board)
  (format t "  1 2 3~%")
  (dotimes (i 3)
    (format t "~a " (1+ i))
    (dotimes (j 3)
      (let ((cell (aref board i j)))
        (format t "~a " (if cell cell #\.))))
    (format t "~%")))

(defun get-move (board player)
  (let ((move nil))
    (loop while (not move)
          do (progn
               (format t "Player ~a's move (row column): " player)
               (finish-output)
               (multiple-value-bind (values row col)
                   (ignore-errors (read-from-string (read-line)))
                 (when (and (integerp row) (integerp col)
                            (<= 1 row 3) (<= 1 col 3))
                   (let ((cell (aref board (- row 1) (- col 1))))
                     (unless cell
                       (setf move (list (- row 1) (- col 1)))))))
               (unless move (format t "Invalid move.~%"))))
    move))

(defun game-over-p (board)
  (let ((winning-lines '((0 1 2) (3 4 5) (6 7 8) ; horizontal lines
                         (0 3 6) (1 4 7) (2 5 8) ; vertical lines
                         (0 4 8) (2 4 6))))      ; diagonal lines
    (or (loop for line in winning-lines
          always (let ((a (aref board (nth 0 line)))
                        (b (aref board (nth 1 line)))
                        (c (aref board (nth 2 line))))
                   (and a b c (eq a b) (eq b c))))
        (notany #'null (loop for i below 3
                         always (loop for j below 3
                                       thereis (null (aref board i j))))))))

(defun tic-tac-toe ()
  (let ((board (make-array '(3 3) :initial-element nil))
        (players '(#\X #\O))
        (turn 0)
        (winner nil))
    (loop while (not (or winner (game-over-p board)))
          do (progn
               (print-board board)
               (let ((player (aref players (mod turn 2))))
                 (setf (aref board (first (get-move board player))
                                  (second (get-move board player)))
                       player))
               (incf turn))
          finally (progn
                    (print-board board)
                    (format t "~%")
                    (cond (winner (format t "Player ~a wins!~%" winner))
                          ((game-over-p board) (format t "It's a tie!~%")))
                    (format t "Game over.~%")))))
