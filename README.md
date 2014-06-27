eval-in-repl.el
--------------------

 This package does what ESS does for R for Inferior Lisp Interaction Mode (ielm). Emacs Speaks Statistics (ESS) package has a nice function called ess-eval-region-or-line-and-step, which is assigned to C-RET.

 This function sends a line or a selected region to the corresponding R, Julia, Stata, etc, shell. It also start up a shell if there is none.

This package implements similar work flow for various read-eval-print-loops (REPLs).


File configuration
--------------------

- eval-in-repl.el
 - eval-in-repl.el is the skeleton package required for each specialized packages for each REPL.
- eval-in-repl-ielm.el
 -  When there is no ielm running, it will be activated. Then the selected region or the last expression (or the current expression the cursor is in) is sent to ielm, and get executed. This will keep track of what has been executed, and should be intuitive for ESS users.
- eval-in-repl-cider.el
-


