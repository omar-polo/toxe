#!/bin/sh

exec emacs -Q \
	-L . \
	-l toxe.el \
	echobot.el
