eval-in-repl.el
--------------------

This package does what ESS does for R for various REPLs, including ielm.

Emacs Speaks Statistics (ESS) package has a nice function called ess-eval-region-or-line-and-step, which is assigned to C-RET. This function sends a line or a selected region to the corresponding shell (R, Julia, Stata, etc) visibly. It also start up a shell if there is none.

This package implements similar work flow for various read-eval-print-loops (REPLs) shown below.


File configurations
--------------------

- eval-in-repl.el
 - Skeleton package required for all specialized packages below.

- eval-in-repl-ielm.el
 - Support for Inferior Emacs Lisp Mode (IELM)

- eval-in-repl-cider.el
 - Support for Clojure via cider.el

- eval-in-repl-slime.el
 - Support for other lisps via slime.el

- eval-in-repl-scheme.el
 - Support for Scheme via scheme.el

- eval-in-repl-python.el
 - Support for Python via python.el

- eval-in-repl-shell.el
 - Support for shell via essh.el



