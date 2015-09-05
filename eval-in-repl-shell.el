;;; eval-in-repl-shell.el --- ESS-like eval for shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.6.0

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
(defalias 'eir-send-to-shell
  (apply-partially 'eir-send-to-repl
                   ;; fun-change-to-repl
                   #'(lambda () (switch-to-buffer-other-window "*shell*"))
                   ;; fun-execute
                   #'comint-send-input)
  "Send expression to *shell* and have it evaluated.")


;;; eir-eval-in-shell
;;;###autoload
(defun eir-eval-in-shell (&optional no-jump-after-eval-p)
  "eval-in-repl for shell."
  (interactive)
  ;; Define local variables
  (let* ((script-window (selected-window))
         (initial-point (point)))
    ;;
    (eir-repl-start "\\*shell\\*" #'shell)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-shell (buffer-substring-no-properties (point) (mark)))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of line
      (end-of-line)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  (eir-send-to-shell (buffer-substring-no-properties (point) (mark)))
	;; If empty, deselect region
	(setq mark-active nil))

      ;; Move to the next statement unless told not to
      (if (not no-jump-after-eval-p)
          (essh-next-code-line)
        ;; Go back to the initial position otherwise
        (goto-char initial-point))

      ;; Switch to the shell
      (switch-to-buffer-other-window "*shell*")
      ;; Switch back to the script window
      (select-window script-window))))


(provide 'eval-in-repl-shell)
;;; eval-in-repl-shell.el ends here

