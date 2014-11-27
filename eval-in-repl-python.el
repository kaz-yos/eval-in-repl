;;; eval-in-repl-python.el --- ESS-like eval for python  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.4.0

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

;; python.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'python)


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
;;;###autoload
(defun eir-eval-in-python ()
  "Evaluates Python expressions"

  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;;
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
	  ;; Add one more character for newline unless at EOF
	  ;; This does not work if the statement asks for an input.
	  (eir-send-to-python (min (+ 1 (point)) (point-max))
			      (mark))
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
      (select-window w-script))))
;;


(provide 'eval-in-repl-python)
;;; eval-in-repl-python.el ends here

