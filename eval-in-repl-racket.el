;;; eval-in-repl-racket.el --- ESS-like eval for racket  -*- lexical-binding: t; -*-

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

;; racket.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'racket)


;;;
;;; RACKET FOR CLOJURE RELATED
;;; eir--racket-jack-in
(defun eir--racket-jack-in ()
  "Invoke racket-jack-in and wait for activation.
If *nrepl-** buffers are remaining, kill them silently.
This function should not be invoked directly."

  (interactive)
  ;; If *nrepl-* buffers exist although *racket-repl* does not, kill them for safety.
  (let* ((nrepl-buffer-names (eir--matching-elements "\\*nrepl-.*\\*$" (mapcar #'buffer-name (buffer-list)))))
    (when nrepl-buffer-names
      ;; Looping over nrepl-buffer-names for side effect
      (mapc (lambda (elt)
	      ;; kill-buffer without asking
	      (let (kill-buffer-query-functions)
		(kill-buffer elt)))
	    nrepl-buffer-names)))
  ;; Activate racket
  (racket-jack-in)
  ;; Wait for connection
  (when (not (racket-connected-p))
    (message "waiting for racket...")
    (sit-for 1)))
;;
;;; eir-send-to-racket
(defun eir-send-to-racket (start end)
  "Sends expression to *racket-repl* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'racket-switch-to-repl-buffer
		    ;; fun-execute
		    ;; #'(lambda () (progn (racket-repl-return t) (racket-repl-return t)))
		    #'racket-repl-return
		    ;; #'(lambda () (racket-repl--send-input t))
		    ))
;;
;;; eir-eval-in-racket
;;;###autoload
(defun eir-eval-in-racket ()
  "This is a customized version of eir-eval-in-repl-lisp for racket."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*racket-repl.*\\*$"
   ;; fun-repl-start
   'eir--racket-jack-in
   ;; fun-repl-send
   'eir-send-to-racket
   ;; defun-string
   "(defn "))
;;



(provide 'eval-in-repl-racket)
;;; eval-in-repl-racket.el ends here

