;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage #:games-http-server
  (:nicknames #:srv)
  (:use #:cl #:hunchentoot)
  (:export #:run-games-server))


