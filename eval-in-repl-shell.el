;;; eval-in-repl-shell.el --- ESS-like eval for shell  -*- lexical-binding: t; -*-

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

;; essh.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'essh)


;;;
;;; SHELL RELATED
;; depends on essh
;; Changed from ESS
;; Auto-scrolling of R console to bottom and Shift key extension
;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/ESSShiftEnter
;;
;;; eir-send-to-shell
(defun eir-send-to-shell (start end)
  "Sends expression to *shell* and have it evaluated."

    (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'(lambda () (switch-to-buffer-other-window "*shell*"))
		    ;; fun-execute
		    #'comint-send-input))
;;
;;; eir-eval-in-shell
;;;###autoload
(defun eir-eval-in-shell ()
  "Evaluates shell expressions in shell scripts."
  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;;
    (eir-repl-start "\\*shell\\*" #'shell)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-shell (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of line
      (end-of-line)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  (eir-send-to-shell (point) (mark))
	;; If empty, deselect region
	(setq mark-active nil))
      ;; Move to the next statement
      (essh-next-code-line)

      ;; Activate shell window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the shell
      (switch-to-buffer-other-window "*shell*")
      ;; Switch back to the script window
      (select-window w-script))))
;;


(provide 'eval-in-repl-shell)
;;; eval-in-repl-shell.el ends here

