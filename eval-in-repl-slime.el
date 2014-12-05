;;; eval-in-repl-slime.el --- ESS-like eval for SLIME  -*- lexical-binding: t; -*-

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

;; slime.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'slime)
(require 'slime-repl)


;;;
;;; SLIME RELATED
;;; eir-send-to-slime
;; send to slime
(defun eir-send-to-slime (start end)
  "Sends expression to *slime-repl* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'slime-switch-to-output-buffer
		    ;; fun-execute
		    #'slime-repl-return))
;;
;;; eir-eval-in-slime
;;;###autoload
(defun eir-eval-in-slime ()
  "This is a customized version of eir-eval-in-repl-lisp for slime."

  (interactive)
  (eir-eval-in-repl-lisp
   ;; repl-buffer-regexp
   "\\*slime-repl.*\\*$"
   ;; fun-repl-start
   #'slime
   ;; fun-repl-send
   #'eir-send-to-slime
   ;; defun-string
   "(defn "))
;;
;;; define keys
;;

(provide 'eval-in-repl-slime)
;;; eval-in-repl-slime.el ends here

