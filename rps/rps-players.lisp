;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RPS; Base: 10 -*-

;;; The players for rock paper scissors.

(in-package :rps)
(defvar *randy* (make-instance 'random-player))

(define-easy-handler (random-move :uri "/games/rps/randy/move"
				  :default-request-type :both)
    ((opponent :real-name "player" :parameter-type 'keyword)
     (opponent-move :real-name "move" :parameter-type 'keyword))
  (let* ((id (incf *move-counter*))
	 (player *randy*)
	 (player-move (play-one-round player opponent id)))
    (when (and (boundp '*reply*) *reply*)
      (setf (hunchentoot:content-type*) "text/plain"))
    (note-result player opponent id (player-result player-move opponent-move))
    (json:encode-json-plist-to-string `(:player ,opponent
					:computer-player :randy
					:move ,opponent-move
					:computer-move ,player-move
					:result ,(opponent-result player-move opponent-move)))))
