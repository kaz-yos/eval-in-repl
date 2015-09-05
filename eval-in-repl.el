;;; eval-in-repl.el --- Consistent ESS-like eval interface for various REPLs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.7.0

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

;; eval-in-repl: Consistent ESS-like eval interface for various REPLs
;;
;; This package does what ESS does for R for various REPLs, including ielm.
;;
;; Emacs Speaks Statistics (ESS) package has a nice function called
;; ess-eval-region-or-line-and-step, which is assigned to C-RET.
;; This function sends a line or a selected region to the corresponding
;; shell (R, Julia, Stata, etc) visibly. It also start up a shell if
;; there is none.
;;
;; This package along with REPL/shell specific packages implement similar
;; work flow for various REPLs.
;;
;; This file alone is not functional. Also require the following depending
;; on your needs.
;;
;; eval-in-repl-ielm.el    for Emacs Lisp    (via ielm)
;; eval-in-repl-cider.el   for Clojure       (via cider.el)
;; eval-in-repl-slime.el   for Common Lisp   (via slime.el)
;; eval-in-repl-geiser.el  for Racket/Scheme (via geiser.el)
;; eval-in-repl-racket.el  for Racket        (via racket-mode.el)
;; eval-in-repl-scheme.el  for Scheme        (via scheme.el and cmuscheme.el)
;; eval-in-repl-hy.el      for Hy            (via hy-mode.el and inf-lisp.el)
;;
;; eval-in-repl-python.el  for Python        (via python.el)
;; eval-in-repl-ruby.el    for Ruby          (via ruby-mode.el, inf-ruby.el, and ess.el)
;; eval-in-repl-sml.el     for Standard ML   (via sml-mode.el and ess.el)
;; eval-in-repl-ocaml.el   for OCaml         (via tuareg.el and ess.el)
;;
;; eval-in-repl-shell.el   for Shell         (via essh.el)
;;
;;
;; See the URL below for installation and configuration instructions,
;; known issues, and version history.
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'dash)
(require 'paredit)



;;;
;;; CUSTOMIZATION VARIABLES
;;
;;; If true, jump after evaluation
;; Contributed by Andrea Richiardi (https://github.com/arichiardi)
(defcustom eir-jump-after-eval t
  "When t enables jumping to next expression after REPL evaluation.

Jumps to the next expression after REPL evaluation if this option
is not-nil (default), stays where it is otherwise."
  :group 'eval-in-repl
  :type 'boolean)


;;;
;;; COMMON ELEMENTS
;;; eir--matching-elements
;; Pure function
(defun eir--matching-elements (regexp lst)
  "Return a list of elements matching the REGEXP in the LIST."
  ;; emacs version of filter (dash.el)
  (-filter
   ;; predicate: non-nil if an element matches the REGEXP
   #'(lambda (elt) (string-match regexp elt))
   lst))


;;; eir-start-repl
;; A function to start a REPL if not already running
;; https://stat.ethz.ch/pipermail/ess-help/2012-December/008426.html
;; http://t7331.codeinpro.us/q/51502552e8432c0426273040
(defun eir-repl-start (repl-buffer-regexp fun-repl-start)
  "Start a REPL if not already available.

Start a REPL using a function specified in FUN-REPL-START,
if a buffer matching REPL-BUFFER-REGEXP is not already available.
Also vertically split the current frame when staring a REPL."

  (interactive)
  ;; Create local variables
  (let* (window1 window2 name-script-buffer name-repl-buffer)
    (if (not (eir--matching-elements
              repl-buffer-regexp
              (mapcar #'buffer-name (buffer-list))))
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

	  ;; REPL on the left (window1)
	  ;; This line is not really necessary because it is already on the left.
	  (set-window-buffer window1 name-repl-buffer)
	  ;; Script on the right (window2)
	  (set-window-buffer window2 name-script-buffer)

	  ;; Select the script window on the right (window2)
	  (select-window window2)))))


;;; eir-send-to-repl
(defun eir-send-to-repl (fun-change-to-repl fun-execute region-string)
  "Send a string to a REPL buffer for execution.

Given a REGION-STRING, switch to the REPL buffer by FUN-CHANGE-TO-REPL,
and execute by FUN-EXECUTE."
  (let* (;; Assign the current buffer
	 (script-window (selected-window)))
    ;; Change other window to REPL
    (funcall fun-change-to-repl)
    ;; Move to end of buffer
    (goto-char (point-max))
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (funcall fun-execute)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil (this is a void function)
    nil))


;;;
;;; COMMON ELEMENT FOR LISP LANGUAGES
;;; eir-eval-in-repl-lisp (used as a skeleton)
(defun eir-eval-in-repl-lisp (repl-buffer-regexp fun-repl-start fun-repl-send defun-string)
  "eval-in-repl function for lisp languages.

Evaluate expression using a REPL specified by REPL-BUFFER-REGEXP.
If not present, a REPL is started using FUN-REPL-START.
Send expression using a function specified in FUN-REPL-SEND.
A function definition is detected by a string specified in DEFUN-STRING
 and handled accordingly."

  (interactive)
  (let* (;; Save current point
	 (initial-point (point)))
    ;; Check for the presence of a REPL buffer
    (eir-repl-start repl-buffer-regexp fun-repl-start)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If there is a selected region, send it to the REPL
	(funcall fun-repl-send (buffer-substring-no-properties (point) (mark)))

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
            ;; Send to REPL
            (funcall fun-repl-send (buffer-substring-no-properties (point) (mark)))
            ;; Go to the next expression
            (forward-sexp))

        ;; If it is not def, do all the following
        ;; Go to the previous position
        (goto-char initial-point)
        ;; Go back one S-exp. (paredit dependency)
        (paredit-backward)
        ;; Loop until at a top-level "(" at column 0
        (while (not (equal (current-column) 0))
          ;; Go back one S-exp. (paredit dependency)
          (paredit-backward))
        ;; Set a mark there
        (set-mark (line-beginning-position))
        ;; Go to the end of the S-exp starting there
        (forward-sexp)
        ;; Send to REPL
        (funcall fun-repl-send (buffer-substring-no-properties (point) (mark))))

      ;; Go to the next expression if configured so
      ;; Contributed by Andrea Richiardi (https://github.com/arichiardi)
      (if eir-jump-after-eval
          (forward-sexp)
        ;; Go back to the initial position otherwise
        (goto-char initial-point)))))


(provide 'eval-in-repl)
;;; eval-in-repl.el ends here
