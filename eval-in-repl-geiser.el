;;; eval-in-repl-geiser.el --- ESS-like eval for geiser  -*- lexical-binding: t; -*-

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

;; geiser.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'geiser-mode)


;;;
;;; GEISER RELATED
;;; eir-send-to-geiser
(defun eir-send-to-geiser (start end)
  "Sends expression to * Racket/Guile REPL * and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'switch-to-geiser
		    ;; fun-execute
		    #'geiser-repl--maybe-send))
;;
;;; eir-eval-in-geiser
;;;###autoload
(defun eir-eval-in-geiser ()
  "This is a customized version of eir-eval-in-repl-lisp for geiser."

  (interactive)
  (eir-eval-in-repl-lisp
   ;; repl-buffer-regexp
   "\\* Racket REPL.*\\*$\\|\\* Guile REPL.*\\*$"
   ;; fun-repl-start
   #'run-geiser
   ;; fun-repl-send
   #'eir-send-to-geiser
   ;; defun-string
   "(define "))
;;



(provide 'eval-in-repl-geiser)
;;; eval-in-repl-geiser.el ends here
