**eval-in-repl: Consistent ESS-like eval interface for various REPLs**
--------------------

This package does what ESS does for R for various REPLs, including ielm.

Emacs Speaks Statistics (ESS) package has a nice function called ess-eval-region-or-line-and-step, which is assigned to C-RET. This function sends a line or a selected region to the corresponding shell (R, Julia, Stata, etc) visibly. It also start up a shell if there is none.

This package implements similar work flow for various read-eval-print-loops (REPLs) shown below.


**Usage**
--------------------

After installation and appropriate configuration (see below), you can use C-RET in a source file to start up an appropriate REPL and evaluate a line, selected region or the current expression depending on the context. The script will be shown on the right, and the REPL on the left. The REPL shows both the code executed and the value the code returned.

![Alt text](screen_shot_ielm.png?raw=true "ielm example")


**Installation**
--------------------

The following files are included in the package. There are respective dependencies.

- eval-in-repl.el
 - Skeleton package required for all specialized packages below.

- eval-in-repl-ielm.el
 - Support for Inferior Emacs Lisp Mode (IELM; part of default emacs installation)

- eval-in-repl-cider.el
 - Support for Clojure via cider.el (depends on cider.el)

- eval-in-repl-slime.el
 - Support for other lisps via slime.el (depends on slime.el)

- eval-in-repl-geiser.el
 - Support for Racket and Guile Scheme via geiser.el (depends on geiser.el)

- eval-in-repl-racket.el
 - Support for Racket via racket-mode.el (depends on racket-mode.el)

- eval-in-repl-scheme.el
 - Support for Scheme via scheme.el (depends on scheme.el and cmuscheme.el; both part of default emacs installation)

- eval-in-repl-python.el
 - Support for Python via python.el (depends on python.el; part of default emacs installation)

- eval-in-repl-shell.el (depends on essh.el)
 - Support for shell via essh.el

- eval-in-repl-sml.el (depends on sml-mode.el and ess.el)
 - Support for Standard ML via sml-mode.el

- eval-in-repl-ruby.el (depends on ruby-mode.el, inf-ruby.el, and ess.el)
 - Support for Ruby via ruby-mode.el

- eval-in-repl-ocaml.el (depends on tuareg.el and ess.el)
 - Support for OCaml via sml-mode.el


It is available on the MELPA repository. You can do the following, then choose and install eval-in-repl.

```
M-x list-packages
```

To configure the MELPA, see this: http://melpa.milkbox.net/#/getting-started


**Configuration**
--------------------

The full configuration is the following. ```eval-in-repl.el``` is always necessary. Require other files as needed and configure the respective mode-specific key bindings.

```lisp
;; require the main file containing common functions
(require 'eval-in-repl)

;; ielm support (for emacs lisp)
(require 'eval-in-repl-ielm)
;; for .el files
(define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; for *scratch*
(define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
;; for M-x info
(define-key Info-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)

;; cider support (for Clojure)
;; (require 'cider) ; if not done elsewhere
(require 'eval-in-repl-cider)
(define-key clojure-mode-map (kbd "<C-return>") 'eir-eval-in-cider)

;; SLIME support (for common lisp)
;; (require 'slime) ; if not done elsewhere
(require 'eval-in-repl-slime)
(add-hook 'lisp-mode-hook
		  '(lambda ()
		     (local-set-key (kbd "<C-return>") 'eir-eval-in-slime)))

;; geiser support (for Racket and Guile Scheme)
;; When using this, turn off racket-mode and scheme supports
;; (require 'geiser) ; if not done elsewhere
(require 'eval-in-repl-geiser)
(add-hook 'geiser-mode-hook
		  '(lambda ()
		     (local-set-key (kbd "<C-return>") 'eir-eval-in-geiser)))

;; racket-mode support (for Racket)
;; (require 'racket-mode) ; if not done elsewhere
;; (require 'eval-in-repl-racket)
;; (define-key racket-mode-map (kbd "<C-return>") 'eir-eval-in-racket)

;; scheme support
;; (require 'scheme)    ; if not done elsewhere
;; (require 'cmuscheme) ; if not done elsewhere
;; (require 'eval-in-repl-scheme)
;; (add-hook 'scheme-mode-hook
;; 	  '(lambda ()
;; 	     (local-set-key (kbd "<C-return>") 'eir-eval-in-scheme)))

;; python support
;; (require 'python) ; if not done elsewhere
(require 'eval-in-repl-python)
(define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python)

;; shell support
;; (require 'essh) ; if not done elsewhere
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
	     (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))

;; sml support
;; (require 'sml-mode) ; if not done elsewhere
(require 'eval-in-repl-sml)
(define-key sml-mode-map (kbd "<C-return>") 'eir-eval-in-sml)
(define-key sml-mode-map (kbd "C-;") 'eir-send-to-sml-semicolon)

;; ruby support
;; (require 'ruby-mode) ; if not done elsewhere
;; (require 'inf-ruby)  ; if not done elsewhere
;; (require 'ess)       ; if not done elsewhere
(require 'eval-in-repl-ruby)
(define-key ruby-mode-map (kbd "<C-return>") 'eir-eval-in-ruby)

;; ocaml support
;; (require 'tuareg) ; if not done elsewhere
(require 'eval-in-repl-ocaml)
(define-key tuareg-mode-map (kbd "<C-return>") 'eir-eval-in-ocaml)
;; function to send a semicolon to OCaml REPL
(define-key tuareg-mode-map (kbd "C-;") 'eir-send-to-ocaml-semicolon)
```

**Known issues**
--------------------

- The first invocation of a cider REPL is slow and sometimes fails.
- If there is no \*cider-repl\*, but \*nrepl-...\* buffers, the latter are killed. This behavior may not be safe.
- The Geiser support is incompatible with the racket-mode support (racket-mode major mode is incompatible with Geiser) and with the scheme-mode support (Geiser will invoke Guile Scheme for .scm files).


**Version histoy**
--------------------

- 2014-11-26 0.4.0 Add Ruby support
- 2014-11-12 0.3.0 Add Standard ML support
- 2014-09-13 0.2.1 Add EOF handling for Python
- 2014-08-30 0.2.0 Add Geiser and Racket support
- 2014-07-06 0.1.1 Delete excess autoload macros, add paredit.el to dependency
- 2014-06-30 0.1.0 First MELPA Release



**Special thanks:**
--------------------

This package was inspired by the wonderful Emacs Speaks Statistics (ESS) package.
