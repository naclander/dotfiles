#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  exec startx
fi


# Auto ls after cd
function cs()
{
  if [ $# -eq 0 ]; then
    cd && ls -AohF --color=auto
  else
    cd "$*" && ls -AohF --color=auto
  fi
}

red='\[\e[1;31m\]'
green='\[\e[1;32m\]'
blue='\[\e[1;36m\]'
magenta='\[\e[1;35m\]'
end='\[\e[m\]'
export PS1="$red\u@\h$end:$blue\W$end$green\\\$$end "


source ~/.bash_aliases

export PATH=/home/hemite/bin:$PATH
export EDITOR="vim" 

# Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline
