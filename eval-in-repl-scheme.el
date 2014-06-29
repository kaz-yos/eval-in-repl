;;; eval-in-repl-scheme.el --- Introduce ESS-like eval to scheme mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.1.0

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

;; scheme.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'scheme)
(require 'cmuscheme)


;;;
;;; SCHEME RELATED
;;; eir-send-to-scheme
;; send to scheme
(defun eir-send-to-scheme (start end)
  "Sends expression to *scheme* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'(lambda ()
			;; Move to the other window
			(other-window 1)
			;; Change to scheme REPL
			(switch-to-scheme t))
		    ;; fun-execute
		    #'comint-send-input))
;;
;;; eir-eval-in-scheme
;;;###autoload
(defun eir-eval-in-scheme ()
  "This is a customized version of eir-eval-in-repl-lisp for scheme."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*scheme\\*"
   ;; fun-repl-start
   'run-scheme
   ;; fun-repl-send
   'eir-send-to-scheme
   ;; defun-string
   "(define "))
;;

(provide 'eval-in-repl-scheme)
;;; eval-in-repl-scheme.el ends here

