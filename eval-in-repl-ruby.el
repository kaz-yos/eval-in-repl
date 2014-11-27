;;; eval-in-repl-ruby.el --- ESS-like eval for ruby  -*- lexical-binding: t; -*-

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

;; ruby.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'ruby-mode)
(require 'inf-ruby)
(require 'ess)


;;;
;;; RUBY-MODE RELATED
;;; eir-send-to-ruby
(defun eir-send-to-ruby (start end)
  "Sends expression to *Ruby* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'run-ruby
		    ;; fun-execute
		    #'comint-send-input))
;;
;;; eir-eval-in-ruby
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_rubyel/
;;;###autoload
(defun eir-eval-in-ruby ()
  "Evaluates Ruby expressions"

  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;;
    (eir-repl-start "\\*ruby\\*" #'run-ruby)


    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-ruby (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of line
      (end-of-line)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  (eir-send-to-ruby (point) (mark))
	;; If empty, deselect region
	(setq mark-active nil))
      ;; Move to the next statement
      (ess-next-code-line)

      ;; Activate ruby window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the inferior ruby 
      (run-ruby)
      ;; Switch back to the script window
      (select-window w-script))))
;;


(provide 'eval-in-repl-ruby)
;;; eval-in-repl-ruby.el ends here

