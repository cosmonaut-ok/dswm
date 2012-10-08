;;; -*- Mode: LISP; Syntax: Common-lisp; Package: dswm.module.swank -*-

;;; Created 2012, Alexander aka 'CosmonauT' Vynnyk <cosmonaut<dot>ok[at]gmail[dot]com>

;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.
;;
;;; Code:

(defpackage :dswm.module.swank
  (:use :cl :dswm :swank)
  (:shadowing-import-from :dswm #:add-hook #:message #:describe-function))

(in-package :dswm.module.swank)

(defun collect-swank-ports (swank-servers-list)
  (if (null swank-servers-list)
      nil
    (cons (cadar swank-servers-list)
     (collect-swank-ports (cdr swank-servers-list)))))

(defun free-swank-port ()
  (let ((swank-ports-list (collect-swank-ports *servers*)))
    (if (not (null swank-ports-list))
	(1+ (eval (cons 'max swank-ports-list)))
      4006)))

(define-dswm-type :swank-port (input prompt)
  (let ((swank-ports-list nil))
    (dolist (port (collect-swank-ports *servers*))
      (setf swank-ports-list (cons (princ-to-string port) swank-ports-list)))
    (or (argument-pop input)
	(completing-read (current-screen)
			 prompt 
			 swank-ports-list
			 :initial-input (princ-to-string (car (last swank-ports-list)))))))

(define-dswm-type :swank-free-port (input prompt)
  (let ((n (or (argument-pop input)
               (read-one-line (current-screen)
			      prompt
			      :initial-input (princ-to-string (free-swank-port))))))
    (when n
      (handler-case
          (parse-integer n)
        (parse-error (c)
          (declare (ignore c))
          (throw 'error "Number required."))))))

(defcommand swank-start-server (port) ((:swank-free-port "Input port number: "))
    (setf *top-level-error-action* :break)
    (if
	(create-server :port port
		       :style *communication-style*
		       :dont-close t)
	(message (format nil "Starting swank.~%To connect, do in Emacs: M-x slime-connect RET RET, then (in-package dswm)."))
      (message "Failed to create swank server")))

(defcommand swank-list-servers () ()
  (let ((servers-list-message
	 (format nil "Running servers list~%--------------------~%")))
    (dolist (port (collect-swank-ports *servers*))
      (setf servers-list-message
	    (concatenate 'string servers-list-message (format nil "On port:~10t~a~%" port))))
    (message servers-list-message)))

(defcommand swank-stop-server (port) ((:swank-port "Input port number: "))
  (if (stop-server (parse-integer port))
      (message (format nil "Server on port ~a stopped" port))
    (message "Failed to stop server")))

(defcommand swank-restart-server (port) ((:swank-port "Input port number: "))
  (if (restart-server :port (parse-integer port)
		      :style *communication-style*
		      :dont-close t)
      (message (format nil "Server on port ~a restarted" port))
    (message "Failed to restart server")))

;; ;; TODO: make possibility to create ssh tunnel to eth0 etc
