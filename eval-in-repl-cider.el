;;; eval-in-repl-cider.el --- Introduce ESS-like eval for cider  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.1.0

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

;; This package does what ESS does for R for cider.
;;
;; Emacs Speaks Statistics (ESS) package has a nice function called
;; ess-eval-region-or-line-and-step, which is assigned to C-RET.
;; This function sends a line or a selected region to the corresponding
;; shell (R, Julia, Stata, etc) visibly. It also start up a shell if there is none.
;;
;; This package implements similar work flow for Clojure via cider.el.
;;
;; When there is no cider REPL running, it will be created. Then the selected
;; region or the last expression (or the current expression the cursor is
;; in) is sent to the REPL, and gets executed. This will keep track of what
;; has been executed, and should be intuitive for ESS users.


;;; Configuration
;; To assign eir-eval-in-cider to C-RET in the clojure mode,
;; add the following to your configuration.
;;
;; (require 'eval-in-repl-cider)
;; (define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'cider)


;;;
;;; CIDER FOR CLOJURE RELATED
;;; eir--cider-jack-in
(defun eir--cider-jack-in ()
  "Invoke cider-jack-in and wait for activation.
If *nrepl-** buffers are remaining, kill them silently.
This function should not be invoked directly."

  (interactive)
  ;; If *nrepl-* buffers exist although *cider-repl* does not, kill them for safety.
  (let* ((nrepl-buffer-names (eir--matching-elements "\\*nrepl-.*\\*$" (mapcar #'buffer-name (buffer-list)))))
    (when nrepl-buffer-names
      (mapcar (lambda (elt)
		;; kill-buffer without asking
		(let (kill-buffer-query-functions)
		  (kill-buffer elt)))
	      nrepl-buffer-names)))
  ;; Activate cider
  (cider-jack-in)
  ;; Wait for connection
  (when (not (cider-connected-p))
    (message "waiting for cider...")
    (sit-for 1)))
;;
;;; eir-send-to-cider
(defun eir-send-to-cider (start end)
  "Sends expression to *cider-repl* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'cider-switch-to-repl-buffer
		    ;; fun-execute
		    ;; #'(lambda () (progn (cider-repl-return t) (cider-repl-return t)))
		    #'cider-repl-return
		    ;; #'(lambda () (cider-repl--send-input t))
		    ))
;;
;;; eir-eval-in-cider
(defun eir-eval-in-cider ()
  "This is a customized version of eir-eval-in-repl-lisp for cider."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*cider-repl.*\\*$"
   ;; fun-repl-start
   'eir--cider-jack-in
   ;; fun-repl-send
   'eir-send-to-cider
   ;; defun-string
   "(defn "))
;;



(provide 'eval-in-repl-cider)
;;; eval-in-repl-cider.el ends here

