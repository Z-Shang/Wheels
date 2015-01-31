# Wheels
Just some random tools

# Truth-Table:
##Usage:

(require 'cl-wheels-logic "truth-table")

(gen-truth-table ARG-LIST EXP-LIST)

e.g.:

(gen-truth-table '(x y z) '( (l-and x y) (l-or y z) (l-and (l-not x) z) ))

##TODO:

Make the output better
