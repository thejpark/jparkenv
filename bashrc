# -*- sh -*-
#
# This file is supposed to be sourced by .bashrc in home dir
#

export SHELL=/bin/bash
export PATH=~/pineenv/bin:$PATH

# If the terminal is not inside windows, use the full colour.
if [ -z "$EMACS" ]; then
    export TERM=xterm-256color
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
	if [ -n "$IN_SCREEN" ]; then
	    PROMPT_COMMAND='echo -ne "\033]0;${PWD/$HOME/~}\007"'
	else
	    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
	fi
    ;;
*)
    ;;
esac

# Set python starty up file, which will be loaded by python command line.
# With this, Python CLI can use 'readline' as default 
export PYTHONSTARTUP=~/pineenv/pythonstartup

# Make it easy to reflect change
alias csrc='. $HOME/.bashrc'

# Disable unused Start/Stop in order to use C-s to do 'i-search
stty -ixon 

# For changing screen window name magically
export PS1='\[\033k\033\\\]\w\$ '
#export PS1='\[\033k\033\\\]\u@\h:\w\$ '
#export PS1='\[\033k\033\\\]\[\e[33m\]\w\[\e[0m\]\n\$ '

# Grep style
export GREP_OPTIONS="--color=always"
export GREP_COLORS="ms=01;37:mc=01;37:sl=:cx=01;30:fn=35:ln=32:bn=32:se=36"

# Less : To show color escape sequence correctly
export LESS="-R"

# Set the default editor to 'ems, a personal script to run emacs with simple configuration
export EDITOR=ems


