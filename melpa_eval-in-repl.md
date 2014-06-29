**A brief summary of what the package does.**
---------------------------------------------

**eval-in-repl: Consistent eval interface for various REPLs**

This package does what ESS does for R for various REPLs, including ielm.

Emacs Speaks Statistics (ESS) package has a nice function called ess-eval-region-or-line-and-step, which is assigned to C-RET. This function sends a line or a selected region to the corresponding shell (R, Julia, Stata, etc) visibly. It also start up a shell if there is none.

This package implements similar work flow for various read-eval-print-loops (REPLs) shown below.


**Your association with the package**
---------------------------------------------
(e.g., are you the maintainer? have you contributed? do you just like the package a lot?).

I am the developer and maintainer of the package.


**A direct link to the package repository.**
---------------------------------------------

https://github.com/kaz-yos/eval-in-repl/

It consists of multiple related files and -pkg.el file which describes the package.


**Relevant communications with the package maintainer**
---------------------------------------------
(e.g., package.el compatibility changes that you have submitted).

I am the maintainer.


**Test that the package builds properly via make recipes/<recipe>.**
---------------------------------------------

Using OS X 10.9.3 and emacs 24.3.91.1, make recipes/eval-in-repl built the package successfully.


**Test that the package installs properly via package-install-file.**
---------------------------------------------

Using M-x package-install-file and the tar file generated in the previous step, it installed without problem, and there were no compilation errors. The package is functioning as expected after installation through the package system.
