;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:REVERSI; Base: 10 -*-

;;; The implementation of reversi.

(in-package :reversi)

;;; To work around SBCL defconstant madness...
;;;
#+(or)
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

#+(or)
(defun random-elt (choices)
  (elt choices (random (length choices))))

#+(or)
(defun last-elt (sequence)
  (if (listp sequence)
      (first (last sequence))
      (elt sequence (1- (length sequence)))))