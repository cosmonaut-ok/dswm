#!/bin/bash

SRCDIR=$(dirname $0)/../..//src/

if [ -z "$LISP" ]; then
	LISP="sbcl"
fi

echo "$LISP"

CONFIG_OPTIONS="--with-lisp=$LISP"

if [ "$(echo $@ | grep "all")" ]; then
	CONFIG_OPTIONS="$CONFIG_OPTIONS --with-manual"
fi

LISP="sbcl"

cd "$SRCDIR"
echo -n "> Cleaning... "
make clean > /dev/null
echo done

echo -n "> Doing autoconf... "
autoconf > /dev/null
echo done

echo -n "> Configuring... "
if [ "$DEBUG" == 1 ]; then
./configure "$CONFIG_OPTIONS"
else
./configure "$CONFIG_OPTIONS" > /dev/null
fi
echo done

echo -n "> Making... "
if [ "$DEBUG" == 1 ]; then
make
else
make > /dev/null
fi
echo done
