;;; eval-in-repl-shell.el --- ESS-like eval for shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.9.4

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


;;;
;;; SHELL RELATED
;;
;;; eir-send-to-shell

(cl-defgeneric eir-shell-execute ()
  nil)

(cl-defmethod eir-shell-execute (&context (major-mode shell-mode))
  (comint-send-input))

(cl-defmethod eir-shell-execute (&context (major-mode term-mode))
  (term-send-input))

(cl-defmethod eir-shell-execute (&context (major-mode vterm-mode))
  (vterm-send-return))

(defalias 'eir-send-to-shell
  (apply-partially 'eir-send-to-repl
                   ;; fun-change-to-repl
                   #'(lambda () (switch-to-buffer-other-window eir-shell-buffer-name))
                   ;; fun-execute
                   #'eir-shell-execute)
  "Send expression to 'eir-shell-buffer-name and have it evaluated.")

(cl-defmethod eir-insert (string &context (major-mode term-mode))
  "Overwrites the default implementation of eir-insert that just calls (insert string)"
  (term-send-string (current-buffer) string))

(cl-defmethod eir-insert (string &context (major-mode vterm-mode))
  "term-send-string seems to work with vterm. Is there another command that should be used instead?"
  (term-send-string (current-buffer) string))

(defun eir--remove-surrounding-stars (string)
  (replace-regexp-in-string "^[*]\\(.+\\)[*]$" "\\1" string))

(cl-defgeneric eir-create-shell (shell-name)
  (error "Could not create shell"))

(cl-defmethod eir-create-shell (shell-name &context (eir-shell-type (eql shell)))
  (shell shell-name))

(cl-defmethod eir-create-shell (shell-name &context (eir-shell-type (eql term)))
  ;; make-term wraps the passed name with asterisks ie *<passed-name>*
  (make-term (eir--remove-surrounding-stars shell-name) eir-shell-term-program))

(cl-defmethod eir-create-shell (shell-name &context (eir-shell-type (eql vterm)))
  ;; vterm messes with the window configuration
  (save-window-excursion
    ;; Start vterm, and force buffer name to stay shell-name (in case
    ;; when vterm-buffer-name-string is customized)
    (with-current-buffer (vterm shell-name)
      (setq-local vterm-buffer-name-string nil))))

;;; eir-eval-in-shell
;;;###autoload
(defun eir-eval-in-shell ()
  "eval-in-repl for shell."
  (interactive)
  ;; Define local variables
  (let* (;; Save current point
	 (initial-point (point)))
    (eir-repl-start (regexp-quote eir-shell-buffer-name)
		    (lambda () (interactive) (eir-create-shell eir-shell-buffer-name))
		    t)

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

