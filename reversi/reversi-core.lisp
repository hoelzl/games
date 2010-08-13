;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:REVERSI; Base: 10 -*-

;;; The implementation of reversi.  This is modelled after Peter
;;; Norvig's implementation in PAIP.

(in-package :reversi)

(define-constant +all-directions+ '(-11 -10 -9 -1 1 9 10 11)
  :test 'equal
  :documentation "All directions in which a player can move.")

(define-constant +empty+ 0
  :documentation  "An empty square.")
(define-constant +black+ 1
  :documentation  "A black piece.")
(define-constant +white+ 2
  :documentation  "A white piece.")
(define-constant +outer+ 3
  :documentation  "A square outside of the board.")

(deftype piece () `(integer ,+empty+ ,+outer+))

(defun name-of (piece)
  (char ".@O?" piece))

(defun opponent (player)
  (if (eql player +black+)
      +white+
      +black+))

(deftype board ()
  '(simple-array piece (100)))

(defun bref (board square)
  (aref board square))

(defun (setf bref) (new-value board square)
  (setf (aref board square) new-value))

(defun copy-board (board)
  (copy-seq board))

(define-constant +all-squares+
    (iter (for i from 11 to 88)
	  (if (<= 1 (mod i 10) 8)
	      (collect i)))
  :test 'equal
  :documentation "The indices of all valid squares.")

(defun initial-board ()
  (let ((board (make-array 100 :element-type 'piece :initial-element +outer+)))
    (iter (for square in +all-squares+)
	  (setf (bref board square) +empty+))
    (setf (bref board 44) +white+ (bref board 45) +black+
	  (bref board 54) +black+ (bref board 55) +white+)
    board))

(defun count-difference (player board)
  (- (count player board)
     (count (opponent player) board)))

(defun print-board (board)
  (format t "~2&    1 2 3 4 5 6 7 8  [~C=~2A ~C=~2A (~@D)]"
	  (name-of +black+) (count +black+ board)
	  (name-of +white+) (count +white+ board)
	  (count-difference +black+ board))
  (iter (for row from 1 to 8)
	(format t "~& ~D " (* 10 row))
	(iter (for col from 1 to 8)
	      (for piece = (bref board (+ col (* 10 row))))
	      (format t "~C " (name-of piece))))
  (format t "~2&"))

(defun valid-p (move)
  "A move in the range 11 to 88 ending in 1-8."
  (and (integerp move)
       (<= 11 move 88)
       (<= 1 (mod move 10) 8)))

(defun find-bracketing-piece (square player board direction)
  "Return the square number of the bracketing piece."
  (let ((piece (bref board square)))
    (cond ((eql piece player) square)
	  ((eql piece (opponent player))
	   (find-bracketing-piece
	    (+ square direction) player board direction))
	  (t nil))))

(defun would-flip (move player board direction)
  "Would this move result in any flips in this direction?  If so,
return the square number of the bracketing piece."
  (let ((square (+ move direction)))
    (and (eql (bref board square) (opponent player))
	 (find-bracketing-piece
	  (+ square direction) player board direction))))

(defun legal-p (move player board)
  "A move into an empty square that flips at least one opponent
piece."
  (and (eql (bref board move) +empty+)
       (some (lambda (direction)
	       (would-flip move player board direction))
	     +all-directions+)))

(defun make-flips (move player board direction)
  "Make any flips in the given direction."
  (let ((bracketer (would-flip move player board direction)))
    (when bracketer
      (iter (for square from (+ move direction) by direction)
	    (until (eql square bracketer))
	    (setf (bref board square) player)))))

(defun make-move (move player board)
  "Update board to reflect move by player."
  (setf (bref board move) player)
  (dolist (direction +all-directions+)
    (make-flips move player board direction))
  board)

#||
(defparameter *board* (initial-board))

(defun mm (move &optional (player +black+) (board *board*))
  (print-board (make-move move player board)))

(defun mb (move)
  (mm move +black+))

(defun mw (move)
  (mm move +white+))
||#

(defun any-legal-move-p (player board)
  "Does player have any legal moves in this position?"
  (some (lambda (move) (legal-p move player board))
	+all-squares+))

(defun next-to-play (board previous-player print)
  "Compute the player to move next, or NIL if nobody can move."
  (let ((opponent (opponent previous-player)))
    (cond ((any-legal-move-p opponent board)
	   opponent)
	  ((any-legal-move-p previous-player board)
	   (when print
	     (format t "~&~C has no moves and must pass."
		     (name-of opponent)))
	   previous-player)
	  (t nil))))

(defun get-move (strategy player board print &optional (repeat 5))
  "Call the player's strategy function to get a move.  Keep calling
at most REPEAT times or until a legal move is made."
  (when print (print-board board))
  (let ((move (funcall strategy player (copy-board board))))
    (cond
      ((and (valid-p move) (legal-p move player board))
       (when print
	 (format t "~&~C moves to ~D."
		 (name-of player)
		 move))
       (make-move move player board))
      ((> repeat 0)
       (warn "Illegal move: ~D" move)
       (get-move strategy player board print (1- repeat)))
      (t
       (error "Too many illegal moves, aborting game.")))))

(defun othello (black-strategy white-strategy &optional (print t))
  "Play a game of Othello.  Return the score where a positive value
means black (the first player) wins."
  (let ((board (initial-board)))
    (catch :exit-othello
      (iter (for player initially +black+ then (next-to-play board player print))
	    (for strategy = (if (eql player +black+)
				black-strategy
				white-strategy))
	    (until (null player))
	    (get-move strategy player board print))
      (when print
	(format t "~&The game is over.  Final result:")
	(print-board board))
      (count-difference +black+ board))))

(defun legal-moves (player board)
  "Returns a list of legal moves for player."
  (iter (for move in +all-squares+)
	(when (legal-p move player board)
	  (collect move))))


;;; Strategies.
;;; ==========

(defun human (player board)
  "A human player for the game of Othello."
  (format t "~&~C to move.  " (name-of player))
  (format t "Legal moves are ~{~D~^, ~}: " (legal-moves player board))
  (let ((move (read)))
    (cond ((integerp move) move)
	  (t (throw :exit-othello move)))))


(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))

(defun maximizer (eval-fun)
  "Return a strategy that will consider every legal move, and chose
  the one for which EVAL-FUN returns the best score."
  (lambda (player board)
    (let* ((moves (legal-moves player board))
	   (scores (mapcar (lambda (move)
			     (funcall eval-fun
				      player
				      (make-move move player (copy-board board))))
			   moves))
	   (best (apply #'max scores)))
      (elt moves (position best scores)))))

(defun maximize-difference (player board)
  "A strategy that maximizes the difference in pieces."
  (funcall (maximizer #'count-difference) player board))

#||
(othello 'maximize-difference 'random-strategy)
||#

(defvar *weights*
  #(0   0   0   0   0   0   0   0   0   0
    0 120 -20  20   5   5  20 -20 120   0
    0 -20 -40  -5  -5  -5  -5 -40 -20   0
    0  20  -5  15   3   3  15  -5  20   0
    0   5  -5   3   3   3   3  -5   5   0
    0   5  -5   3   3   3   3  -5   5   0
    0  20  -5  15   3   3  15  -5  20   0
    0 -20 -40  -5  -5  -5  -5 -40 -20   0
    0 120 -20  20   5   5  20 -20 120   0
    0   0   0   0   0   0   0   0   0   0))

(defun weighted-squares (player board)
  "Sum of the weights of player's square minus opponent's."
  (let ((opponent (opponent player)))
    (iter (for i in +all-squares+)
	  (when (eql (bref board i) player)
	    (sum (aref *weights* i)))
	  (when (eql (bref board i) opponent)
	    (sum (- (aref *weights* i)))))))

(defun maximize-weighted-squares (player board)
  "A strategy that maximizes the difference in weighted squares."
  (funcall (maximizer #'weighted-squares) player board))


;;; Minimax.
;;; =======

(define-constant +winning-value+ most-positive-fixnum)
(define-constant +losing-value+ most-negative-fixnum)

(defun final-value (player board)
  "Is this a win, loss or draw for the player."
  (ecase (signum (count-difference player board))
    (-1 +losing-value+)
    ( 0 0)
    ( 1 +winning-value+)))

(defun minimax (player board ply eval-fun)
  "Find the best move for PLAYER according to EVAL-FUN searching PLY
levels deep and backing up values."
  (if (= ply 0)
      (funcall eval-fun player board)
      (let ((moves (legal-moves player board)))
	(if (null moves)
	    (let ((opponent  (opponent player)))
	      (if (any-legal-move-p opponent board)
		  (- (minimax opponent board (1- ply) eval-fun))
		  (final-value player board)))
	    (let ((best-move nil)
		  (best-val nil))
	      (dolist (move moves)
		(let* ((board2 (make-move move player (copy-board board)))
		       (val (- (minimax (opponent player) board2 (1- ply) eval-fun))))
		  (when (or (null best-val) (> val best-val))
		    (setf best-val val
			  best-move move))))
	      (values best-val best-move))))))

(defun minimax-searcher (ply eval-fun)
  "A strategy that searches PLY levels using EVAL-FUN."
  (lambda (player board)
    (multiple-value-bind (value move)
	(minimax player board ply eval-fun)
      (declare (ignore value))
      move)))

#||
(othello (minimax-searcher 3 'count-difference) (maximizer 'count-difference))
||#

;;; Alpha-Beta.
;;; ==========

#+(or)
(defun alpha-beta (player board achievable cutoff ply eval-fun)
  "Find the best move for PLAYER according to EVAL-FUN searching PLY
levels deep and backing up values."
  (if (= ply 0)
      (funcall eval-fun player board)
      (let ((moves (legal-moves player board)))
	(if (null moves)
	    (let ((opponent  (opponent player)))
	      (if (any-legal-move-p opponent board)
		  (- (alpha-beta opponent board
				 (- cutoff) (- achievable)
				 (1- ply) eval-fun))
		  (final-value player board)))
	    (iter (with best-move = (first moves))
		  (for move in moves)
		  (let* ((board2 (make-move move player
					    (copy-board board)))
			 (val (- (alpha-beta (opponent player) board2
					     (- cutoff) (- achievable)
					     (1- ply) eval-fun))))
		    (when (> val achievable)
		      (setf achievable val
			    best-move move)))
		  (until (>= achievable cutoff))
		  (finally (return (values achievable best-move))))))))


(defun alpha-beta (player board achievable cutoff ply eval-fun)
  "Find the best move for PLAYER according to EVAL-FUN searching PLY
levels deep and backing up values."
  (if (= ply 0)
      (funcall eval-fun player board)
      (let ((moves (legal-moves player board)))
	(if (null moves)
	    (let ((opponent  (opponent player)))
	      (if (any-legal-move-p opponent board)
		  (- (alpha-beta opponent board
				 (- cutoff) (- achievable)
				 (1- ply) eval-fun))
		  (final-value player board)))
	    (iter (with best-move = (first moves))
		  (for move in moves)
		  (for board2 = (make-move move player
					   (copy-board board)))
		  (for val = (- (alpha-beta (opponent player) board2
					    (- cutoff) (- achievable)
					    (1- ply) eval-fun)))
		  (when (> val achievable)
		    (setf achievable val
			  best-move move))
		  (until (>= achievable cutoff))
		  (finally (return (values achievable best-move))))))))

(defun alpha-beta-searcher (ply eval-fun)
  "A strategy that uses alpha-beta search, PLY levels deep with EVAL-FUN."
  (lambda (player board)
    (multiple-value-bind (value move)
	(alpha-beta player board
		    +losing-value+ +winning-value+
		    ply eval-fun)
      (declare (ignore value))
      move)))


;;; Random Alpha-Beta.
;;; =================

(defun random-alpha-beta (player board achievable cutoff ply eval-fun)
  "Find the best move for PLAYER according to EVAL-FUN searching PLY
levels deep and backing up values."
  (if (= ply 0)
      (funcall eval-fun player board)
      (let ((moves (shuffle (legal-moves player board))))
	(if (null moves)
	    (let ((opponent  (opponent player)))
	      (if (any-legal-move-p opponent board)
		  (- (random-alpha-beta opponent board
					(- cutoff) (- achievable)
					(1- ply) eval-fun))
		  (final-value player board)))
	    (iter (with best-move = (first moves))
		  (for move in moves)
		  (for board2 = (make-move move player
					   (copy-board board)))
		  (for val = (- (random-alpha-beta (opponent player) board2
						   (- cutoff) (- achievable)
						   (1- ply) eval-fun)))
		  (when (> val achievable)
		    (setf achievable val
			  best-move move))
		  (until (>= achievable cutoff))
		  (finally (return (values achievable best-move))))))))

(defun random-alpha-beta-searcher (ply eval-fun)
  "A strategy that uses random-alpha-beta search, PLY levels deep with EVAL-FUN."
  (lambda (player board)
    (multiple-value-bind (value move)
	(random-alpha-beta player board
			   +losing-value+ +winning-value+
			   ply eval-fun)
      (declare (ignore value))
      move)))

#||
(defun compare-strategies (black-strategy white-strategy &optional (iterations 30))
  (let ((black-wins 0)
	(draws 0))
    (dotimes (i iterations)
      (let ((result (othello black-strategy white-strategy nil)))
	(cond ((> result 0) (incf black-wins))
	      ((zerop result) (incf draws)))
	(print result)))
    (format t "~&Black wins: ~A times.~%Draws:      ~A" black-wins draws)))
(compare-strategies (random-alpha-beta-searcher 3 'weighted-squares)
		    (alpha-beta-searcher 3 'weighted-squares))
||#