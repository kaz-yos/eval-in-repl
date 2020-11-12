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

(defun eir-send-to-shell (string)
  (let* ((mode (buffer-local-value 'major-mode (get-buffer eir-shell-buffer-name)))
	 (funs (cond ((eq mode 'shell-mode) (list
					     #'(lambda (string)
						 (goto-char (point-max))
						 (insert string))
					     #'comint-send-input))
		     ((eq mode 'term-mode) (list
					    #'(lambda (string)
						(term-send-string (current-buffer) string))
					    #'term-send-input))
		     (t (error "was expecting shell or term mode")))))
    (with-current-buffer eir-shell-buffer-name
      (funcall (car funs) string)
      (funcall (cadr funs)))
    ))

(defun eir-create-shell ()
  ;; TODO needs to be interactive? I remember something about shell not playing well...?
  (interactive)
  (cond ((eq eir-shell-mode 'shell)
	 (shell eir-shell-buffer-name))
	((eq eir-shell-mode 'term)
	 ;; make-term wraps the passed name with asterisks ie *<passed-name>*
	 ;; TODO remove these asterisks if present
	 (make-term eir-shell-buffer-name eir-shell-term-program))))

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

