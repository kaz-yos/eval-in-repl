;;; eval-in-repl-sly.el --- ESS-like eval for sly  -*- lexical-binding: t; -*-

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

;; sly-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'sly)


;;;
;;; EMACS LISP RELATED
;;; eir-send-to-sly
(defalias 'eir-send-to-sly
  (apply-partially 'eir-send-to-repl
                   ;; fun-change-to-repl
                   (lambda () (sly-mrepl #'switch-to-buffer))
                   ;; fun-execute
                   #'sly-mrepl-return)
  "Send expression to *sly* and have it evaluated.")


;;; eir-eval-in-sly
;;;###autoload
(defun eir-eval-in-sly ()
  "eval-in-repl for Sly."
  (interactive)
  (eir-eval-in-repl-lisp
   ;; repl-buffer-regexp
   "\\*sly-mrepl.*\\*"
   ;; fun-repl-start
   #'sly
   ;; fun-repl-send
   #'eir-send-to-sly
   ;; defun-string
   "(defun "))
;;

(provide 'eval-in-repl-sly)
;;; eval-in-repl-sly.el ends here
