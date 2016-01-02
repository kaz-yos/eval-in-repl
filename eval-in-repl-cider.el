;;; eval-in-repl-cider.el --- ESS-like eval for cider  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.9.1

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

;; cider.el-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'cider)


;;;
;;; CIDER FOR CLOJURE RELATED
;;; eir--cider-jack-in
(defun eir--cider-jack-in ()
  "Invoke cider-jack-in and wait for activation.

If *nrepl-** buffers are remaining, kill them silently.
This function should not be invoked directly."
  (interactive)
  ;; If *nrepl-* buffers exist although *cider-repl* does not, kill them for safety.
  (let* ((nrepl-buffer-names (eir--matching-elements
                              "\\*cider-repl.*\\*$"
                              (mapcar #'buffer-name (buffer-list)))))
    (when nrepl-buffer-names
      ;; Looping over nrepl-buffer-names for side effect
      (mapc (lambda (elt)
	      ;; kill-buffer without asking
	      (let (kill-buffer-query-functions)
		(kill-buffer elt)))
	    nrepl-buffer-names)))
  ;; Activate cider
  (cider-jack-in)
  ;; Wait for connection
  (message "Please wait for cider REPL. It will take some time."))


;;; eir-send-to-cider
(defalias 'eir-send-to-cider
  (apply-partially 'eir-send-to-repl
                   ;; fun-change-to-repl
                   #'cider-switch-to-repl-buffer
                   ;; fun-execute
                   #'cider-repl-return)
  "Send expression to *cider-repl* and have it evaluated.")


;;; eir-eval-in-cider
;;;###autoload
(defun eir-eval-in-cider ()
  "eval-in-repl for cider."
  (interactive)
  ;; Override defcustom eir-always-split-script-window
  ;; This option is not functional with cider currently.
  (let* ((eir-always-split-script-window nil))
    (eir-eval-in-repl-lisp
     ;; repl-buffer-regexp
     "\\*cider-repl.*\\*$"
     ;; fun-repl-start
     #'eir--cider-jack-in
     ;; fun-repl-send
     #'eir-send-to-cider
     ;; defun-string
     "(defn "
     ;; exec-in-script
     t)))


(provide 'eval-in-repl-cider)
;;; eval-in-repl-cider.el ends here
