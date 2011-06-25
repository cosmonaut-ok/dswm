;test.lisp --script

(defparameter *argv*
  (or 
   #+SBCL (cdr *posix-argv*)
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   #+CLISP *args*
   nil))

(format t "~a~%" *argv*)
