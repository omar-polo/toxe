#!/bin/sh

exec emacs -Q						\
	-L .						\
	-l toxe.el					\
	--eval "(split-window-below)"			\
	--eval "(other-window 1)"			\
	--eval "(switch-to-buffer \"*Messages*\")"	\
	--eval "(other-window 1)"			\
	echobot.el
