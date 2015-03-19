;;; DBUS integration for DSWM
;;;
;;; Copyright 2014 Russell Sim <russell.sim@gmail.com>
;;; Copyright 2015 Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@zoho.com>
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package #:dswm)

(export
 '(make-object-from-introspection
	 object-invoke
	 interface-name
	 list-object-interfaces
	 with-introspected-object
	 make-future
	 futurep
	 attach
	 finish
	 alet
	 alet*
	 future-finished-p
	 future-values
	 with-future
	 with-futures
	 open-connection
	 close-connection))

;; ;;;; dbus.lisp

(defvar *event-base* nil)
(defvar *connection* nil)
(defvar *dbus* nil)
(defvar *in-queue* (make-mailbox)
  "The list of responses being returned to DSWM.")
(defvar *message-dispatch* (make-hash-table))
(defvar *internal-dispatch* (make-hash-table))
(defvar *responses* (make-hash-table))
(defvar *thread* nil)


(defun dbus-event-loop ()
  (iolib.multiplex:event-dispatch (connection-event-base *connection*)))

(defun open-connection ()
  (setf *event-base* (make-instance 'event-base))
  (setf *connection*
        (dbus:open-connection *event-base* (system-server-addresses) :if-failed :error))
  (authenticate (supported-authentication-mechanisms *connection*) *connection*)
  (let ((serial (dbus:invoke-method *connection* "Hello"
                                    :path "/org/freedesktop/DBus"
                                    :interface "org.freedesktop.DBus"
                                    :destination "org.freedesktop.DBus" :asynchronous t)))
    (flet ((set-dbus (message)
             (setf *dbus* (make-instance 'bus :name message
                                              :connection *connection*))))
      (setf (gethash serial *message-dispatch*) #'set-dbus)))
  (let ((*default-special-bindings*
          (acons '*connection* *connection*
                 *default-special-bindings*)))
    (setf *thread* (make-thread 'dbus-event-loop
                                :name "dbus")))
  ;; Hook into DSWM loop
  (add-hook  *internal-loop-hook* 'handle-pending-messages)
  *connection*)

(defun close-connection ()
  ;; Remove hook from DSWM loop
  (remove-hook  *internal-loop-hook* 'handle-pending-messages)
  (when (and *thread* (thread-alive-p *thread*))
    (destroy-thread *thread*))
  (when *connection* (dbus:close-connection *connection*))
  (when *event-base* (close *event-base*))

  (setf *dbus* nil
        *event-base* nil
        *connection* nil
        *thread* nil))


(defmethod (setf connection-pending-messages) :around (message (connection connection))
  "Queue messages in a thread safe queue instead of on the connection
class."
  (let ((message (car message)))
    (etypecase message
      (method-return-message
       (let ((fn (gethash (message-reply-serial message) *internal-dispatch*)))
         (if fn
             (funcall fn message)
             (post-mail message *in-queue*))))
      (signal-message nil)
      (error-message
       (let ((fn (gethash (message-reply-serial message) *internal-dispatch*)))
         (if fn
             (funcall fn message)
             (post-mail message *in-queue*)))))))

(defvar *last-message* nil)
(defun handle-pending-messages ()
  (loop :for message = (read-mail *in-queue*)
        :when message
          :do (format t "Received ~a~%" message)
        :while message
        :do (etypecase message
              (method-return-message (let ((fn (gethash (message-reply-serial message) *message-dispatch*)))
                                       (print (message-signature message))
                                       (cond
                                         ((functionp fn)
                                          (funcall fn message)
                                          (setf *last-message* message))
                                         ((futurep fn)
                                          (finish fn message)
                                          (setf *last-message* message)))))
              (signal-message nil)
              (error-message (error 'method-error :arguments (message-body message))))))

(defun send-outgoing-message (message)
  (send-message message *connection*))

(defun make-object (connection path destination interfaces)
  (let ((object (make-instance 'object :connection connection :path path :destination destination)))
    (dolist (interface interfaces)
      (setf (object-interface (interface-name interface) object) interface))
    object))

(defun invoke-method (member
                      &key (connection *connection*) path signature arguments interface
                        destination no-reply no-auto-start
                        (endianness :little-endian)
                        (dispatch *message-dispatch*))
  (let* ((serial (connection-next-serial connection))
         (message
          (encode-message endianness :method-call
                          (logior (if no-reply message-no-reply-expected 0)
                                  (if no-auto-start message-no-auto-start 0))
                          1 serial path
                          interface member nil nil
                          destination nil signature arguments)))
    ;; dispatch message to different thread
    (let ((future (make-future)))
      (setf (gethash serial dispatch) future)
      (interrupt-thread *thread* 'send-outgoing-message message)
      future)))


(defun fetch-introspection-document (path destination &key (connection *connection*))
  (invoke-method "Introspect"
                 :path path
                 :connection connection
                 :destination destination
                 :interface "org.freedesktop.DBus.Introspectable"))


(defun signature-for-method (method-name interface-name object)
  (method-signature (interface-method method-name (object-interface interface-name object))))


(defun object-invoke (object interface-name method-name &rest args)
  (let ((future (make-future)))
        (attach
          (invoke-method method-name
                         :path (object-path object)
                         :interface interface-name
                         :destination (object-destination object)
                         :signature (signature-for-method method-name interface-name object)
                         :arguments args)
          (lambda (reply)
            (finish future (values-list (message-body reply)))))
        future))


(defun make-object-from-introspection (path destination &key (connection *connection*))
  (let ((future (make-future))
        (object (fetch-introspection-document path destination)))
    (attach object
            (lambda (message)
              (finish future
                      (make-object connection path destination
                                   (parse-introspection-document
                                    (car (message-body message)))))))
    future))


(defmacro with-introspected-object ((name path destination) &body forms)
  (with-gensyms (object)
    `(alet ((,object (make-object-from-introspection ,path ,destination)))
       (flet ((,name (interface-name method-name &rest args)
                (apply #'object-invoke ,object interface-name method-name args)))
         ,@forms))))


(defun nm-current-state ()
  (let ((future (make-object-from-introspection
                 "/org/freedesktop/NetworkManager"
                 "org.freedesktop.NetworkManager")))
    (loop
      :do (handle-pending-messages)
      :until (future-finished-p future))
    (apply #'values (future-values future))))


(defmacro wait-for ((&key (timeout 5)) &body body)
  `(let ((future (progn ,@body))
         (until (+ (get-universal-time) ,timeout)))
    (loop
      :do (handle-pending-messages)
      :if (future-finished-p future)
        :do (return)
      :if (> (get-universal-time) until)
        :do (error "timed out."))
    (future-values future)))

(defun futures-map (function futures-list cb)
  (flet ((call-back (result)
           (declare (ignore result))
           (when (every #'future-finished-p futures-list)
             (funcall cb (mapcar #'future-values futures-list)))))
    (dolist (future futures-list)
      (attach (funcall function future) #'call-back))))


(defmacro with-futures ((var futures-list) &body body)
  (with-gensyms (callback futures future)
    `(let ((,futures ,futures-list))
        (flet ((,callback (result)
                 (declare (ignore result))
                 (when (every (lambda (f)
                                (or (not (futurep f)) (future-finished-p f)))
                              ,futures)
                   (let ((,var (mapcar (lambda (f)
                                         (if (futurep f)
                                             (future-values f)
                                             (values-list (ensure-list f))))
                                       ,futures)))
                       ,@body))))
          (dolist (,future ,futures)
            (attach ,future #',callback))))))

(defmacro with-future ((var) &body body)
  `(let ((,var (make-future)))
     ,@body
     ,var))
