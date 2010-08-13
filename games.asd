;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; An implementation of some games for testing the adaptation system.

(in-package :cl-user)

(defpackage :games-asd
  (:use :cl :asdf)
  (:export #:*games-version*))

(in-package :games-asd)

(defvar *games-version* "0.0.1"
  "A string denoting the current version of the games package.")

(defsystem :games
  :serial t
  :version #.*games-version*
  :depends-on (:alexandria :cl-json :cl-ppcre :hunchentoot :iterate)
  :components ((:module "server"
	        :serial t
		:depends-on ()
		:components ((:file "package")
			     (:file "hunchentoot")))
	       (:module "rps"
		:serial t
		:components ((:file "package")
			     (:file "rps-core")
			     (:file "rps-players")))

	       (:module "reversi"
		:serial t
		:components ((:file "package")
			     (:file "utilities")
			     (:file "reversi-core")
			     (:file "reversi-players")))))
