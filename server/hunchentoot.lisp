;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :games-http-server)

(defvar *games-server*)

(defun run-games-server (&key (port 8080))
  (unless (and (boundp '*games-server*) *games-server*)
    (setf *games-server* (make-instance 'acceptor :port port)))
  (start *games-server*))

#||
(run-games-server)
(setf *header-stream* *standard-output*)
||#
