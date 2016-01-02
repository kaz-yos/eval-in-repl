;;; eval-in-repl-python.el --- ESS-like eval for python  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.9.1

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
;;; CUSTOMIZATION VARIABLES
;;
;;; If true, do not invoke REPL if not available
(defcustom eir-python-dedicated nil
  "When t, create a dedicated REPL.

Currently only supports a non-dedicated REPL"
  :group 'eval-in-repl
  :type 'boolean)


;;;
;;; PYTHON-MODE RELATED
;;; eir-python-witch-to-shell
(defun eir-python-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (switch-to-buffer-other-window
   (buffer-name (process-buffer (python-shell-get-or-create-process))) t))


;;; eir-send-to-python
(defalias 'eir-send-to-python
  (apply-partially 'eir-send-to-repl
                   ;; fun-change-to-repl
                   #'python-shell-switch-to-shell
                   ;; fun-execute
                   #'comint-send-input)
  "Send expression to *Python* and have it evaluated.")


;;; eir-run-python
(defun eir-run-python ()
  "Modified version of run-python

This one does not disturb the window layout."
  (interactive)
  (let* ((repl-buffer-name
          (python-shell-make-comint (python-shell-parse-command)
                                    (python-shell-get-process-name eir-python-dedicated)
                                    nil)))
    ;; Start REPL and save buffer name
    ;; Bring up REPL
    (set-window-buffer (selected-window) repl-buffer-name))
  (sit-for 1))


;;; eir-eval-in-python
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_pythonel/
;;;###autoload
(defun eir-eval-in-python ()
  "eval-in-repl for Python."
  (interactive)
  ;; Define local variables
  (let* (;; Save current point
	 (initial-point (point)))
    ;;
    (eir-repl-start "*Python*" #'eir-run-python)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-python (buffer-substring-no-properties (point) (mark)))

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
	  (eir-send-to-python (buffer-substring-no-properties
                               (min (+ 1 (point)) (point-max))
                               (mark)))
	;; If empty, deselect region
	(setq mark-active nil))

      ;; Move to the next statement code if jumping
      (if eir-jump-after-eval
          (python-nav-forward-statement)
        ;; Go back to the initial position otherwise
        (goto-char initial-point)))))


(provide 'eval-in-repl-python)
;;; eval-in-repl-python.el ends here

