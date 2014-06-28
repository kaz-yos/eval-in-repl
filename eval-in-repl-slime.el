;;; eval-in-repl-slime.el --- Introduce ESS-like eval to SLIME  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience

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

;; This package does what ESS does for R for Inferior Lisp Interaction
;; Mode (ielm).
;;
;; Emacs Speaks Statistics (ESS) package has a nice function called
;; ess-eval-region-or-line-and-step, which is assigned to C-RET.
;; This function sends a line or a selected region to the corresponding
;; shell (R, Julia, Stata, etc) visibly. It also start up a shell if there is none.
;;
;; This package implements similar work flow for Emacs Lisp (ielm).
;; When there is no ielm running, it will be activated. Then the selected
;; region or the last expression (or the current expression the cursor is
;; in) is sent to ielm, and gets executed. This will keep track of what
;; has been executed, and should be intuitive for ESS users.


;;; Configuration
;; To assign eir-eval-in-slime to C-RET in the lisp mode,
;; add the following to your configuration.
;;
;; (add-hook 'lisp-mode-hook
;; 	  '(lambda ()
;; 	     (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'slime)


;;;
;;; SLIME RELATED
;;; eir-send-to-slime
;; send to slime
(defun eir-send-to-slime (start end)
  "Sends expression to *slime-repl* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'slime-switch-to-output-buffer
		    ;; fun-execute
		    #'slime-repl-return))
;;
;;; eir-eval-in-slime
(defun eir-eval-in-slime ()
  "This is a customized version of eir-eval-in-repl-lisp for slime."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*slime-repl.*\\*$"
   ;; fun-repl-start
   'slime
   ;; fun-repl-send
   'eir-send-to-slime
   ;; defun-string
   "(defn "))
;;
;;; define keys
;;

(provide 'eval-in-repl-slime)
;;; eval-in-repl-slime.el ends here

