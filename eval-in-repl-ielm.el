;;; eval-in-repl-ielm.el --- Introduce ESS-like behavior to .el files and ielm  -*- lexical-binding: t; -*-

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
;; R, Julia, Stata, etc shell. It also start up a shell if there is none.
;;
;; This package implements similar work flow for Emacs Lisp (ielm).
;; When there is no ielm running, it will be activated. Then the selected
;; region or the last expression (or the current expression the cursor is
;; in) is sent to ielm, and get executed. This will keep track of what
;; has been executed, and should be intuitive for ESS users.


;;; Configuration
;; To assign eir-eval-in-ielm to C-RET in specific emacs-lisp modes,
;; add the following to your configuration.
;;
;; For .el files
;; (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;;
;; For *scratch*
;; (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;;
;; For M-x info
;; (define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)


;;; Code:

;;;
;;; Require the common skeleton package
(require 'eval-in-repl)

;;;
;;; EMACS LISP RELATED
;;; eir-send-to-ielm
(defun eir-send-to-ielm (start end)
  "Sends expression to *ielm* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'(lambda () (switch-to-buffer-other-window "*ielm*"))
		    ;; fun-execute
		    #'ielm-return))
;;
;;; eir-eval-in-ielm
(defun eir-eval-in-ielm ()
  "This is a customized version of eir-eval-in-repl-lisp for ielm."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*ielm\\*"
   ;; fun-repl-start
   #'ielm
   ;; fun-repl-send
   #'eir-send-to-ielm
   ;; defun-string
   "(defun "))
;;

(provide 'eval-in-repl-ielm)
;;; eval-in-repl-ielm.el ends here

