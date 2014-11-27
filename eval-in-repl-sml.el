;;; eval-in-repl-sml.el --- ESS-like eval for Standard ML  -*- lexical-binding: t; -*-

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

;; sml-mode.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'sml-mode)
(require 'ess)


;;;
;;; SML RELATED
;; depends on sml-mode
;;
;;; eir-send-to-sml
(defun eir-send-to-sml (start end)
  "Sends expression to *sml* and have it evaluated."

    (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'(lambda () (switch-to-buffer-other-window "*sml*"))
		    ;; fun-execute
		    #'comint-send-input))
;;
;;; eir-eval-in-sml
;;;###autoload
(defun eir-eval-in-sml ()
  "Evaluates SML expressions in SML files."
  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;; If buffer named *sml* is not found, invoke sml-run
    (eir-repl-start "\\*sml\\*" #'sml-run)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-sml (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of line
      (end-of-line)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  (eir-send-to-sml (point) (mark))
	;; If empty, deselect region
	(setq mark-active nil))
      ;; Move to the next statement
      (ess-next-code-line)

      ;; Activate sml window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the sml
      (switch-to-buffer-other-window "*sml*")
      ;; Switch back to the script window
      (select-window w-script))))
;;
(defun eir-send-to-sml-semicolon ()
  "Sends a semicolon to *sml* and have it evaluated."
  (interactive)

  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string ";"))

    ;; Change other window to REPL
    (switch-to-buffer-other-window "*sml*")
    ;; Move to end of buffer
    (goto-char (point-max))
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (comint-send-input)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil (this is a void function)
    nil))


(provide 'eval-in-repl-sml)
;;; eval-in-repl-sml.el ends here

