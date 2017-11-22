;;; eval-in-repl-shell.el --- ESS-like eval for shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Kazuki YOSHIDA, Terje Larsen

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>, Terje Larsen <terlar@gmail.com>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.9.6

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

;; sh-script.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'sh-script)
(require 'comint)


;;;
;;; SHELL RELATED

;;;###autoload
(defun eir-shell-repl ()
  "Start or switch to `sh-shell' REPL buffer."
  (interactive)
  (let* ((dest-sh (symbol-name sh-shell))
         (buffer-name (format "*shell [%s]*" dest-sh))
         ;; This variable is read by shell function and determines which shell
         ;; is launched.
         (explicit-shell-file-name dest-sh))
    (shell buffer-name)))

;;; eir-send-to-shell
(defalias 'eir-send-to-shell
  (apply-partially 'eir-send-to-repl
                   ;; fun-change-to-repl
                   #'eir-shell-repl
                   ;; fun-execute
                   #'comint-send-input)
  "Send expression to *shell* and have it evaluated.")

;;; eir-eval-in-shell
;;;###autoload
(defun eir-eval-in-shell ()
  "Provides eval-in-repl for `sh-script' files."
  (interactive)
  ;; Define local variables
  (let* (;; Save current point
        (initial-point (point))
        ;; Shell buffer
        (buffer-name (format "*shell [%s]*" (symbol-name sh-shell))))
    ;;
    (eir-repl-start (regexp-quote buffer-name) #'eir-shell-repl t)

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

      ;; Move to the next statement code if jumping
      (if eir-jump-after-eval
          (eir-next-code-line)
        ;; Go back to the initial position otherwise
        (goto-char initial-point)))))


(provide 'eval-in-repl-shell)
;;; eval-in-repl-shell.el ends here
