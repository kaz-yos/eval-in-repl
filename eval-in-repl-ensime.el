;;; eval-in-repl-ensime.el --- ESS-like eval for ensime  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; ensime.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'ensime)


;;;
;;; CIDER FOR CLOJURE RELATED
;;; eir--ensime-jack-in
(defun eir--ensime-jack-in ()
  "Invoke ensime-jack-in and wait for activation.
If *nrepl-** buffers are remaining, kill them silently.
This function should not be invoked directly."

  (interactive)
  ;; If *nrepl-* buffers exist although *ensime-repl* does not, kill them for safety.
  (let* ((nrepl-buffer-names (eir--matching-elements "\\*nrepl-.*\\*$" (mapcar #'buffer-name (buffer-list)))))
    (when nrepl-buffer-names
      ;; Looping over nrepl-buffer-names for side effect
      (mapc (lambda (elt)
	      ;; kill-buffer without asking
	      (let (kill-buffer-query-functions)
		(kill-buffer elt)))
	    nrepl-buffer-names)))
  ;; Activate ensime
  (ensime-jack-in)
  ;; Wait for connection
  (when (not (ensime-connected-p))
    (message "waiting for ensime...")
    (sit-for 1)))
;;
;;; eir-send-to-ensime
(defun eir-send-to-ensime (start end)
  "Sends expression to *ensime-repl* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'ensime-switch-to-repl-buffer
		    ;; fun-execute
		    ;; #'(lambda () (progn (ensime-repl-return t) (ensime-repl-return t)))
		    #'ensime-repl-return
		    ;; #'(lambda () (ensime-repl--send-input t))
		    ))
;;
;;; eir-eval-in-ensime
;;;###autoload
(defun eir-eval-in-ensime ()
  "This is a customized version of eir-eval-in-repl-lisp for ensime."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*ensime-repl.*\\*$"
   ;; fun-repl-start
   'eir--ensime-jack-in
   ;; fun-repl-send
   'eir-send-to-ensime
   ;; defun-string
   "(defn "))
;;



(provide 'eval-in-repl-ensime)
;;; eval-in-repl-ensime.el ends here

