;;; exercism.el --- Minor mode for working through Exercism.io tracks    -*- lexical-binding: t -*-

;; Copyright © 2018 Andrew Kirkpatrick <ubermonk+exercism.el@gmail.com>

;; Author: Andrew Kirkpatrick <ubermonk+exercism.el@gmail.com>
;; Created: 30 Sep 2018
;; Keywords: learning, convenience
;; URL: http://github.com/spacebat/exercism.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; See https://exercism.io
;;
;;
;;; Code:

(defvar exercism-executable "exercism")

(defun exercism--skewer-case (string)
  (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase string)))

(defun exercism--lisp-identifier (string)
  (intern (exercism--skewer-case string)))

(defun exercism--next-line ()
  (when (< (point) (point-max))
    (beginning-of-line)
    (set-mark (point))
    (condition-case err
        (forward-line 1)
      (end-of-buffer))
    (if (= (mark) (point))
        nil
      (buffer-substring-no-properties (mark) (point)))))

(cl-defun exercism--call-with-lines (func &key start end (chomp t) collect omit-nulls)
  (when start
    (goto-char start))
  (cl-loop with result
     for raw-line = (exercism--next-line)
     as line = (if chomp (s-chomp raw-line) raw-line)
     while (and line (or (not end) (< (point) end)))
     do (setf result (funcall func line))
     when (and collect (or (not omit-nulls) result))
     collect result))

(cl-defmacro exercism--with-lines ((var &key start end (chomp t) collect (omit-nulls t)) &body body)
  (declare (indent 1))
  (assert (and var (symbolp var)))
  `(let ((func (lambda (,var)
                 ,@body)))
     (exercism--call-with-lines func :start ,start :end ,end :chomp ,chomp :collect ,collect :omit-nulls ,omit-nulls)))

(defun exercism--get-config ()
  (exercism--with-exec ("configure" "--show")
    (exercism--with-lines (line :start (point-min) :collect t)
      (when (string-match "^\\([^:]+\\):[[:space:]]+\\(?:(.*)[[:space:]]+\\)?\\(.*?\\)[[:space:]]*$" line)
        (cons (exercism--lisp-identifier (match-string 1 line)) (match-string 2 line))))))

(defvar exercism-config nil)

(defun exercism-config (&optional refresh)
  (if (or refresh (not exercism-config))
      (setf exercism-config (exercism--get-config))
    exercism-config))

(defun exercism--exec (args))

(defvar exercism--exec-status nil)

(defun exercism--default-status-handler (status)
  (unless (zerop status)
    (error "exercism exited with %s status" status)))

(defvar exercism-status-handler 'exercism--default-status-handler)

(cl-defun exercism--call-with-exec (func args &key status-handler)
  (with-temp-buffer
    (let ((exercism--exec-status (apply 'call-process exercism-executable nil t nil args)))
      (when exercism-status-handler
        (funcall exercism-status-handler exercism--exec-status))
      (funcall func))))

(cl-defmacro exercism--with-exec ((&rest args) &body body)
  (declare (indent 1))
  `(let ((func (lambda () ,@body)))
     (exercism--call-with-exec func (list ,@args))))

(defun exercism-workspace ()
  (cdr (assoc 'workspace (exercism-config))))

(defun exercism-tracks ()
  (mapcar 'file-name-nondirectory
          (--select (and (not (string-match "^\\." (file-name-nondirectory it)))
                         (file-directory-p it))
                    (directory-files (exercism-workspace) t))))

(defun exercism-track-directory (track-name)
  (expand-file-name track-name (exercism-workspace)))

(defun exercism-exercise-directory (track-name exercise-name)
  (expand-file-name exercise-name (exercism-track-directory track-name)))

(defun exercism-track-exercises (track-name)
  (mapcar 'file-name-nondirectory
          (--select (and (not (string-match "^\\." (file-name-nondirectory it)))
                         (file-directory-p it))
                    (directory-files (exercism-track-directory track-name) t))))

(defun exercism-exercise-open (track-name exercise-name)
  (exercism--with-exec ("open" (exercism-exercise-directory track-name exercise-name))))

(defun exercism-parse-directory (directory)
  ;; obtain the track-name and exercise-name from the directory if possible
  )

;;; Interactive Commands

(defun exercism-submit ()
  "Submit the current buffer's file to exercism"
  (interactive)
  (assert (buffer-file-name)) ;; could check that this is in fact within an exercise directory
  (exercism--with-exec ("submit" (buffer-file-name))
    (message "%s" (buffer-substring-no-properties (point-min) (point-max)))))

(defun exercism-open ()
  "Submit the current buffer's exercism webpage"
  (interactive)
  (let ((dir (if buffer-file-name
                 (f-dirname (f-dirname (expand-file-name (buffer-file-name))))
               (expand-file-name default-directory))))
    (assert dir) ;; could check that this is in fact within an exercise directory
    (message "opening %s" dir)
    (exercism--with-exec ("open" dir)
      (message "%s" (buffer-substring-no-properties (point-min) (point-max))))))

(defun exercism-show-configuration ()
  (interactive)
  (message "Exercism configuration:\n%s"
           (mapconcat 'identity
                      (cl-loop for (key . val) in (exercism-config)
                         collect (format "%12s: %s" key val))
                      "\n")))

;;; Mode definition

(define-minor-mode exercism-mode
  "Get your exercisms in the right places."
  :lighter " Exer"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c S") 'exercism-submit)
            (define-key map (kbd "C-c O") 'exercism-open)
            (define-key map (kbd "C-c C") 'exercism-show-configuration)
            map))

(provide 'exercism)
