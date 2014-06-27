;;; eval-in-repl.el --- Send code to REPL for evaluation  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience

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

;; Emacs Speaks Statistics (ESS) package has a nice function called
;; ess-eval-region-or-line-and-step, which is assigned to C-RET.
;; This function sends a line or a selected region to the corresponding
;; R, Julia, Stata, etc shell. It also start up a shell if there is none.
;;
;; This package implements similar work flow for Emacs Lisp (ielm), Clojure
;; (via cider), other lisps (via SLIME), Scheme (via scheme-mode), Python
;; (via python.el), and shell (via essh.el).

;;; Code:


;;;
;;; COMMON ELEMENTS
;;; eir--matching-elements
(defun eir--matching-elements (regexp list)
  "Return a list of elements matching the REGEXP in the LIST."
  ;; emacs version of filter
  (delete-if-not
   ;; predicate: non-nil if an element matches the REGEXP
   (lambda (elt) (string-match regexp elt))
   list))
;;
;;
;;; eir-start-repl
;; A function to start a REPL if not already available
;; https://stat.ethz.ch/pipermail/ess-help/2012-December/008426.html
;; http://t7331.codeinpro.us/q/51502552e8432c0426273040
(defun eir-repl-start (repl-buffer-regexp fun-repl-start)
  "Start a REPL if not already available.

Start a REPL using a function specified in FUN-REPL-START,
if a buffer matching REPL-BUFFER-REGEXP is not already available.
Also vertically split the current frame when staring a REPL."

  (interactive)
  ;; Create local variables
  (let* (window1 window2 name-script-buffer name-repl-buffer)
    (if (not (eir--matching-elements repl-buffer-regexp (mapcar #'buffer-name (buffer-list))))
	(progn
	  ;; C-x 1 Keep only the window from which this function was called.
	  (delete-other-windows)

	  ;; Make window1 keep the selected (only) window
	  (setq window1 (selected-window))
	  ;; Make name-script-buffer keep the selected (only) buffer
	  (setq name-script-buffer (buffer-name))
	  ;; (split-window &optional WINDOW SIZE SIDE)
	  ;; Split window1 (only one) without size, and create a new window on the right.
	  ;; Use the return value (new window) for window2.
	  ;; window1: left (still selected), window2: right
	  (setq window2 (split-window window1 nil "right"))

	  ;; Activate the REPL (Interactive functions are used)
	  (call-interactively fun-repl-start)

	  ;; Make name-repl-buffer keep the selected buffer (REPL)
	  ;; This does not work for python/clojure
	  (setq name-repl-buffer (buffer-name))

	  ;; ;; REPL on the left (window1)  ; Not really necessary.
	  ;; (set-window-buffer window1 name-repl-buffer)
	  ;; Script on the right (window2)
	  (set-window-buffer window2 name-script-buffer)

	  ;; Select the script window on the right (window2)
	  (select-window window2)
	  ))))
;;
;;
;;; eir-send-to-repl
(defun eir-send-to-repl (start end fun-change-to-repl fun-execute)
  "Sekeleton function to be used with a wrapper.

Sends expression to a repl and have it evaluated."

  (interactive "r")
  (let* (;; Assign the current buffer
	 (script-window (selected-window))
	 ;; Assign the region as a string
	 (region-string (buffer-substring-no-properties start end)))

    ;; Change other window to REPL
    (funcall fun-change-to-repl)
    ;; Move to end of buffer
    (end-of-buffer)
    ;; Insert the string
    (insert region-string)
    ;; Execute
    (funcall fun-execute)
    ;; Come back to the script
    (select-window script-window)
    ;; Return nil
    nil
    ))


;;;
;;; COMMON ELEMENTS FOR LISP LANGUAGES
;;; eir-eval-in-repl-lisp (used as a skeleton)
(defun eir-eval-in-repl-lisp (repl-buffer-regexp fun-repl-start fun-repl-send defun-string)
    "Skeleton function to be used with a wrapper.

Evaluates expression using a REPL specified by REPL-BUFFER-REGEXP.
If not present, a REPL is started using FUN-REPL-START.
Sends expression using a function specified in FUN-REPL-SEND.
A function definition is detected by a string specified in DEFUN-STRING
 and handled accordingly."

  (interactive)
  (let* (;; Save current point
	 (initial-point (point)))

    ;; defined in 200_eir-misc-functions-and-bindings.el
    (eir-repl-start repl-buffer-regexp fun-repl-start)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send to ielm
	(funcall fun-repl-send (point) (mark))
      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Check if the first word is def (function def)
      (if (looking-at defun-string)
	  ;; Use eval-defun if on defun
	  (progn
	    ;; Set a mark there
	    (set-mark (line-beginning-position))
	    ;; Go to the end
	    (forward-sexp)
	    ;; Send to ielm
	    (funcall fun-repl-send (point) (mark))
	    ;; Go to the next expression
	    (forward-sexp))
	;; If it is not def, do all the following
	;; Go to the previous position
	(goto-char initial-point)
	;; Go back one S-exp. (paredit dependency)
	(paredit-backward)
	;; Loop
	(while (not (equal (current-column) 0))
	  ;; Go back one S-exp. (paredit dependency)
	  (paredit-backward))
	;; Set a mark there
	(set-mark (line-beginning-position))
	;; Go to the end of the S-exp starting there
	(forward-sexp)
	;; Eval the S-exp before
	(funcall fun-repl-send (point) (mark))
	;; Go to the next expression
	(forward-sexp)
	))))
;;

;;;
;;; EMACS LISP RELATED
;;; eir-send-to-ielm
(defun eir-send-to-ielm (start end)
  "Sends expression to *ielm* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'(lambda () (switch-to-buffer-other-window "*ielm*"))
		    ;; fun-execute
		    #'ielm-return))
;;
;;; eir-eval-in-ielm
(defun eir-eval-in-ielm ()
  "This is a customized version of eir-eval-in-repl-lisp for ielm."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*ielm\\*"
   ;; fun-repl-start
   #'ielm
   ;; fun-repl-send
   #'eir-send-to-ielm
   ;; defun-string
   "(defun "))
;;
;;; define keys
;; .el files
(define-key emacs-lisp-mode-map		(kbd "<C-return>") 'eir-eval-in-ielm)
;; *scratch*
(define-key lisp-interaction-mode-map	(kbd "<C-return>") 'eir-eval-in-ielm)
;; M-x info
(define-key Info-mode-map		(kbd "<C-return>") 'eir-eval-in-ielm)



;;;
;;; CIDER FOR CLOJURE RELATED
;;; eir-cider-jack-in
(defun eir-cider-jack-in ()
  "Invoke cider-jack-in and wait for activation.
If *nrepl-** buffers are remaining, kill them silently.
This function should not be invoked directly."

  (interactive)
  ;; If *nrepl-* buffers exist although *cider-repl* does not, kill them for safety.
  (let* ((nrepl-buffer-names (eir--matching-elements "\\*nrepl-.*\\*$" (mapcar #'buffer-name (buffer-list)))))
    (when nrepl-buffer-names
      (mapcar (lambda (elt)
		;; kill-buffer without asking
		(let (kill-buffer-query-functions)
		  (kill-buffer elt)))
	      nrepl-buffer-names)))
  ;; Activate cider
  (cider-jack-in)
  ;; Wait for connection
  (when (not (cider-connected-p))
    (message "waiting for cider...")
    (sit-for 1)))
;;
;;; eir-send-to-cider
(defun eir-send-to-cider (start end)
  "Sends expression to *cider-repl* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'cider-switch-to-repl-buffer
		    ;; fun-execute
		    ;; #'(lambda () (progn (cider-repl-return t) (cider-repl-return t)))
		    #'cider-repl-return
		    ;; #'(lambda () (cider-repl--send-input t))
		    ))
;;
;;; eir-eval-in-cider
(defun eir-eval-in-cider ()
  "This is a customized version of eir-eval-in-repl-lisp for cider."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*cider-repl.*\\*$"
   ;; fun-repl-start
   'eir-cider-jack-in
   ;; fun-repl-send
   'eir-send-to-cider
   ;; defun-string
   "(defn "))
;;
;;; define keys
(define-key clojure-mode-map		(kbd "<C-return>") 'eir-eval-in-cider)



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
(defun eir-eval-in-slime ()
  "This is a customized version of eir-eval-in-repl-lisp for slime."

  (interactive)
  (eir-eval-in-repl-lisp	; defined in 200_eir-misc-functions-and-bindings.el
   ;; repl-buffer-regexp
   "\\*slime-repl.*\\*$"
   ;; fun-repl-start
   'slime
   ;; fun-repl-send
   'eir-send-to-slime
   ;; defun-string
   "(defn "))
;;
;;; define keys
(add-hook 'lisp-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))



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
;;; define keys
(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))



;;;
;;; PYTHON-MODE RELATED
;;; eir-send-to-python
(defun eir-send-to-python (start end)
  "Sends expression to *Python* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'python-shell-switch-to-shell
		    ;; fun-execute
		    #'comint-send-input))
;;
;;; eir-eval-in-python
;; http://www.reddit.com/r/emacs/comments/1h4hyw/selecting_regions_pythonel/
(defun eir-eval-in-python ()
  "Evaluates Python expressions"

  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;; defined in 200_eir-misc-functions-and-bindings.el
    (eir-repl-start "*Python*" #'run-python)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-python (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of statment
      (python-nav-end-of-statement)
      ;; Go to the end of block
      (python-nav-end-of-block)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  ;; Add one more character for newline
	  ;; This does not work if a fuction asks for an input.
	  ;; In that case, just select the line.
	  (eir-send-to-python (+ (point) 1) (mark))
	;; If empty, deselect region
	(setq mark-active nil))
      ;; Move to the next statement
      (python-nav-forward-statement)

      ;; Activate shell window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the shell
      (python-shell-switch-to-shell)
      ;; Switch back to the script window
      (select-window w-script)
      )))
;;; define keys
(define-key python-mode-map		(kbd "<C-return>") 'eir-eval-in-python)



;;;
;;; SHELL RELATED
;; depends on essh
;; Changed from ESS
;; Auto-scrolling of R console to bottom and Shift key extension
;; http://www.kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/ESSShiftEnter
;;; eir-send-to-shell
(defun eir-send-to-shell (start end)
  "Sends expression to *shell* and have it evaluated."

    (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'(lambda () (switch-to-buffer-other-window "*shell*"))
		    ;; fun-execute
		    #'comint-send-input))
;;
;;; eir-eval-in-shell
(defun eir-eval-in-shell ()
  "Evaluates shell expressions in shell scripts."
  (interactive)
  ;; Define local variables
  (let* (w-script)

    ;; defined in 200_eir-misc-functions-and-bindings.el
    (eir-repl-start "\\*shell\\*" #'shell)

    ;; Check if selection is present
    (if (and transient-mark-mode mark-active)
	;; If selected, send region
	(eir-send-to-shell (point) (mark))

      ;; If not selected, do all the following
      ;; Move to the beginning of line
      (beginning-of-line)
      ;; Set mark at current position
      (set-mark (point))
      ;; Go to the end of line
      (end-of-line)
      ;; Send region if not empty
      (if (not (equal (point) (mark)))
	  (eir-send-to-shell (point) (mark))
	;; If empty, deselect region
	(setq mark-active nil))
      ;; Move to the next statement
      (essh-next-code-line)

      ;; Activate shell window, and switch back
      ;; Remeber the script window
      (setq w-script (selected-window))
      ;; Switch to the shell
      (switch-to-buffer-other-window "*shell*")
      ;; Switch back to the script window
      (select-window w-script)
      )))
;;
;;; define keys
(add-hook 'sh-mode-hook		; For shell script mode
          '(lambda()
	     (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))


;;;


(provide 'eval-in-repl)
;;; eval-in-repl.el ends here

