;;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'cl-lib))

(defvar *test-data-root* (expand-file-name
                          "data/"
                          (file-name-directory load-file-name)))

(defvar *test-temp-dir*)
(defvar *test-git-repo*)

(defun set-up-temp-dir ()
  (setf *test-temp-dir* (make-temp-file "magit-p4-test" t)
        default-directory *test-temp-dir*))

(defun tear-down-temp-dir ()
    (delete-directory *test-temp-dir* t))

(defun set-up-git-repo ()
  (let ((src (expand-file-name "_git" *test-data-root*))
        (target (expand-file-name "magit-p4/.git" *test-temp-dir*)))
    (copy-directory src target nil t t)
    (setf *test-git-repo* (file-name-directory target))))

(defun buffer-contains-p (string &optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (search-forward string nil t)))))

(defun simulate-keys (keys)
  (setf unread-command-events (append unread-command-events
                                      (listify-key-sequence (kbd keys)))))

(cl-defmacro without-transient-debugger (&body body)
  ;; Buttercup installs its own debugger, so we need to make sure transient doesn't overwrite it
  `(let ((current-debugger debugger))
     (cl-letf (((symbol-function 'transient--exit-and-debug)
               (lambda (&rest args)
                 (transient--emergency-exit :debugger)
                 (apply current-debugger args))))
      ,@body)))
