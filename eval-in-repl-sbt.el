;;; eval-in-repl-sbt.el --- ESS-like eval for sbt  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.2.0

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

;; sbt-mode.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'sbt-mode)


;;;
;;; SBT FOR SCALA RELATED
;;; eir-send-to-sbt
(defun eir-send-to-sbt (start end)
  "Sends expression to *sbt* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'sbt-start
		    ;; #'(lambda () (switch-to-buffer-other-window "*ielm*"))
		    ;; fun-execute
		    #'comint-send-input))
;;
;;; eir-eval-in-sbt
;;;###autoload
(defun eir-eval-in-sbt ()
  "Evaluates Scala expressions"

  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;;
    (eir-repl-start "*sbt*.*" #'sbt-start)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-sbt (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Move to the end of line
      (end-of-line)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  ;; Add one more character for newline
	  ;; This does not work if a fuction asks for an input.
	  ;; In that case, just select the line.
	  (eir-send-to-sbt (+ (point) 1) (mark))
	;; If empty, deselect region
	(setq mark-active nil))
      ;; Move to the next line
      (forward-line)

      ;; Activate shell window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the shell
      (sbt-start)
      ;; Switch back to the script window
      (select-window w-script))))
;;



(provide 'eval-in-repl-sbt)
;;; eval-in-repl-sbt.el ends here

