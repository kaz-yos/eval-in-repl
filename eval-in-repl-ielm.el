;;; ess-ielm.el --- Introduce ESS-like behavior to .el files and ielm  -*- lexical-binding: t; -*-

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
;; To assign eielm-eval-in-ielm to C-RET in specific emacs-lisp modes,
;; add the following to your configuration.
;;
;; For .el files
;; (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eielm-eval-in-ielm)
;;
;; For *scratch*
;; (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eielm-eval-in-ielm)
;;
;; For M-x info
;; (define-key Info-mode-map (kbd "<C-return>") 'eielm-eval-in-ielm)


;;; Code:

;;;
;;; COMMON ELEMENTS
;;; eielm--matching-elements
(defun eielm--matching-elements (regexp list)
  "Return a list of elements matching the REGEXP in the LIST."
  ;; emacs version of filter
  (delete-if-not
   ;; predicate: non-nil if an element matches the REGEXP
   (lambda (elt) (string-match regexp elt))
   list))
;;
;;
;;; eielm-start-repl
;; A function to start a REPL if not already available
;; https://stat.ethz.ch/pipermail/ess-help/2012-December/008426.html
;; http://t7331.codeinpro.us/q/51502552e8432c0426273040
(defun eielm-repl-start (repl-buffer-regexp fun-repl-start)
  "Start a REPL if not already available.

Start a REPL using a function specified in FUN-REPL-START,
if a buffer matching REPL-BUFFER-REGEXP is not already available.
Also vertically split the current frame when staring a REPL."

  (interactive)
  ;; Create local variables
  (let* (window1 window2 name-script-buffer name-repl-buffer)
    (if (not (eielm--matching-elements repl-buffer-regexp (mapcar #'buffer-name (buffer-list))))
	(progn
	  ;; C-x 1 Keep only the window from which this function was called.
	  (delete-other-windows)

	  ;; Make window1 keep the selected (only) window
	  (setq window1 (selected-window))
	  ;; Make name-script-buffer keep the selected (only) buffer
	  (setq name-script-buffer (buffer-name))
	  ;; (split-window &optional WINDOW SIZE SIDE)
	  ;; Split window1 (only one) without size, and create a new window on the right.
	  ;; Use the return value (new window) for window2.
	  ;; window1: left (still selected), window2: right
	  (setq window2 (split-window window1 nil "right"))

	  ;; Activate the REPL (Interactive functions are used)
	  (call-interactively fun-repl-start)

	  ;; Make name-repl-buffer keep the selected buffer (REPL)
	  ;; This does not work for python/clojure
	  (setq name-repl-buffer (buffer-name))

	  ;; ;; REPL on the left (window1)  ; Not really necessary.
	  ;; (set-window-buffer window1 name-repl-buffer)
	  ;; Script on the right (window2)
	  (set-window-buffer window2 name-script-buffer)

	  ;; Select the script window on the right (window2)
	  (select-window window2)
	  ))))
;;
;;
;;; eielm-send-to-repl
(defun eielm-send-to-repl (start end fun-change-to-repl fun-execute)
  "Sekeleton function to be used with a wrapper.

Sends expression to a repl and have it evaluated."

  (interactive "r")
  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Change other window to REPL
    (funcall fun-change-to-repl)
    ;; Move to end of buffer
    (end-of-buffer)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (funcall fun-execute)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))


;;;
;;; COMMON ELEMENTS FOR LISP LANGUAGES
;;; eielm-eval-in-repl-lisp (used as a skeleton)
(defun eielm-eval-in-repl-lisp (repl-buffer-regexp fun-repl-start fun-repl-send defun-string)
    "Skeleton function to be used with a wrapper.

Evaluates expression using a REPL specified by REPL-BUFFER-REGEXP.
If not present, a REPL is started using FUN-REPL-START.
Sends expression using a function specified in FUN-REPL-SEND.
A function definition is detected by a string specified in DEFUN-STRING
 and handled accordingly."

  (interactive)
  (let* (;; Save current point
	 (initial-point (point)))

    ;; defined in 200_eielm-misc-functions-and-bindings.el
    (eielm-repl-start repl-buffer-regexp fun-repl-start)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send to ielm
	(funcall fun-repl-send (point) (mark))
      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Check if the first word is def (function def)
      (if (looking-at defun-string)
	  ;; Use eval-defun if on defun
	  (progn
	    ;; Set a mark there
	    (set-mark (line-beginning-position))
	    ;; Go to the end
	    (forward-sexp)
	    ;; Send to ielm
	    (funcall fun-repl-send (point) (mark))
	    ;; Go to the next expression
	    (forward-sexp))
	;; If it is not def, do all the following
	;; Go to the previous position
	(goto-char initial-point)
	;; Go back one S-exp. (paredit dependency)
	(paredit-backward)
	;; Loop
	(while (not (equal (current-column) 0))
	  ;; Go back one S-exp. (paredit dependency)
	  (paredit-backward))
	;; Set a mark there
	(set-mark (line-beginning-position))
	;; Go to the end of the S-exp starting there
	(forward-sexp)
	;; Eval the S-exp before
	(funcall fun-repl-send (point) (mark))
	;; Go to the next expression
	(forward-sexp)
	))))
;;

;;;
;;; EMACS LISP RELATED
;;; eielm-send-to-ielm
(defun eielm-send-to-ielm (start end)
  "Sends expression to *ielm* and have it evaluated."

  (eielm-send-to-repl start end
		    ;; fun-change-to-repl
		    #'(lambda () (switch-to-buffer-other-window "*ielm*"))
		    ;; fun-execute
		    #'ielm-return))
;;
;;; eielm-eval-in-ielm
(defun eielm-eval-in-ielm ()
  "This is a customized version of eielm-eval-in-repl-lisp for ielm."

  (interactive)
  (eielm-eval-in-repl-lisp	; defined in 200_eielm-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*ielm\\*"
   ;; fun-repl-start
   #'ielm
   ;; fun-repl-send
   #'eielm-send-to-ielm
   ;; defun-string
   "(defun "))



(provide 'ess-ielm)
;;; ess-ielm.el ends here

