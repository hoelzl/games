;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage #:rock-paper-scissors
  (:nicknames #:rps)
  (:use #:cl #:hunchentoot)
  (:export #:player #:opponents
	   #:move #:result
	   #:note-result #:player-result #:opponent-result

	   #:new-opponent-for
	   #:find-opponent-for

	   #:play-one-round #:*move-counter*
	   
	   #:level-0-player
	   #:random-player))
