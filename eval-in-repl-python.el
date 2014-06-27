;;; eval-in-repl-python.el --- Introduce ESS-like eval for python  -*- lexical-binding: t; -*-

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

;; This package does what ESS does for R for python.
;;
;; Emacs Speaks Statistics (ESS) package has a nice function called
;; ess-eval-region-or-line-and-step, which is assigned to C-RET.
;; This function sends a line or a selected region to the corresponding
;; R, Julia, Stata, etc shell. It also start up a shell if there is none.
;;
;; This package implements similar work flow for python.
;; When there is no python running, it will be created. Then the selected
;; region or the current expression  is sent to python, and get executed.
;; This will keep track of what has been executed, and should be intuitive
;; for ESS users.


;;; Configuration
;; To assign eir-eval-in-python to C-RET in the clojure mode,
;; add the following to your configuration.
;;
;; For clojure mode
;; (define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python)


;;; Code:

;;;
;;; Require the common skeleton package
(require 'eval-in-repl)


;;;
;;; PYTHON-MODE RELATED
;;; eir-send-to-python
(defun eir-send-to-python (start end)
  "Sends expression to *Python* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'python-shell-switch-to-shell
		    ;; fun-execute
		    #'comint-send-input))
;;
;;; eir-eval-in-python
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_pythonel/
(defun eir-eval-in-python ()
  "Evaluates Python expressions"

  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;; defined in 200_eir-misc-functions-and-bindings.el
    (eir-repl-start "*Python*" #'run-python)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-python (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of statment
      (python-nav-end-of-statement)
      ;; Go to the end of block
      (python-nav-end-of-block)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  ;; Add one more character for newline
	  ;; This does not work if a fuction asks for an input.
	  ;; In that case, just select the line.
	  (eir-send-to-python (+ (point) 1) (mark))
	;; If empty, deselect region
	(setq mark-active nil))
      ;; Move to the next statement
      (python-nav-forward-statement)

      ;; Activate shell window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the shell
      (python-shell-switch-to-shell)
      ;; Switch back to the script window
      (select-window w-script)
      )))
;;


(provide 'eval-in-repl-python)
;;; eval-in-repl-python.el ends here

