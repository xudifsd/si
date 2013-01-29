#!/bin/bash

# using code at http://bit.ly/Q2L9hK to format my code
for i in $@
do
	cat $1 | indent.lisp > /tmp/lispindent
	mv /tmp/lispindent $1
done
