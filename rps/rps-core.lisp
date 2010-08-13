;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RPS; Base: 10 -*-

;;; The implementation of rock paper scissors games.

(in-package :rps)

;;; The PLAYER class.

(defclass player ()
  ((opponents :initform (make-hash-table)
	      :accessor opponents))
  (:documentation
   "The superclass of all players for RPS."))

(deftype move ()
  '(member :rock :paper :scissors))

(deftype result ()
  '(member :win :lose :draw))

(defconstant $all-moves (if (boundp '$all-moves)
			    $all-moves
			    #(:rock :paper :scissors)))

(defgeneric move (player opponent id)
  (:documentation
   "Make one move against OPPONENT.  ID is an identifier for the
   current move. The result must be of type MOVE."))

(defgeneric note-result (player opponent id result)
  (:documentation
   "Inform the PLAYER that its move against OPPONENT in game ID had
outcome RESULT."))

;;; Handling Opponents.
;;; ------------------

(defgeneric opponent-key (opponent-name)
  (:documentation
   "Return a key for identifying an opponent.")
  (:method ((opponent-name string))
    (let ((opponent-name (string-trim " \t\n\r\f" opponent-name)))
      (when (string= opponent-name "")
	(error "Opponent name cannot be empty."))
      (intern (string-upcase opponent-name) :keyword))))

(defgeneric new-opponent-for (player opponent-name)
  (:documentation
   "Create a new opponent for PLAYER.")
  (:argument-precedence-order opponent-name player)
  (:method (player (opponent string))
    (new-opponent-for player (opponent-key opponent))))

(defgeneric find-opponent-for (player opponent)
  (:documentation
   "Finds the opponent for player.")
  (:argument-precedence-order opponent player)
  
  (:method (player (opponent symbol))
    (or
     (gethash opponent (opponents player))
     (setf (gethash opponent (opponents player))
	   (new-opponent-for player opponent))))
  (:method (player (opponent string))
    (find-opponent-for player (opponent-key opponent))))

;;; Playing a Single Round.

(defvar *move-counter* 0)

(defun play-one-round
    (player opponent-name &optional (id (incf *move-counter*)))
  (let ((opponent (find-opponent-for player opponent-name)))
    (move player opponent id)))

;;; Level-0 Player

(defclass level-0-player (player)
  ()
  (:documentation
   "A LEVEL-0-PLAYER plays its moves without taking into account the
   behavior of its opponents.  (The term is taken from a book about
   poker strategy.)"))

(defmethod new-opponent-for ((player level-0-player) opponent)
  "No need to keep information about the opponent for a level-0
  player."
  opponent)

(defmethod note-result ((player level-0-player) opponent id result) 
  (declare (ignore opponent id result))
  nil)

(defgeneric player-result (player-move opponent-move)
  (:documentation
   "Return the result from the player's point of view.")
  (:method (pm om)
    (if (eql pm om)
	:draw
	:lose))
  (:method ((pm (eql :scissors)) (om  (eql :paper)))
    :win)
  (:method ((pm (eql :paper)) (om (eql :rock)))
    :win)
  (:method ((pm (eql :rock)) (om (eql :scissors)))
    :win))

(defun opponent-result (player-move opponent-move)
  "Return the result from the opponent's point of view."
  (player-result opponent-move player-move))

;;; Random Player.

(defclass random-player (level-0-player)
  ()
  (:documentation
   "A player that always picks a move at random."))

(defmethod move ((player random-player) opponent id)
  (declare (ignore opponent id))
  (aref $all-moves (random 3)))
