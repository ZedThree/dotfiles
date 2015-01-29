# .bashrc

export LC_ALL=C
export LANG=C

# export PATH=/hwdisks/home/ph781/bin/:$PATH
export PYTHONPATH=/hwdisks/home/ph781/BOUT-dev/tools/pylib/:$PYTHONPATH

if [[ $(hostname) != "laser" ]]; then
    source /hwdisks/data/modules/bin/use_modules.sh
    module load netcdf/4.0.1
    module load python
    module load openmpi/1.4.5
    module load git
	module load emacs
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions


HISTFILESIZE=1000000000
HISTSIZE=1000000
shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

# pretty colours
BC_BLACK="\033[030m"
BC_RED="\033[031m"
BC_GREEN="\033[032m"
BC_YELLOW="\033[033m"
BC_BLUE="\033[034m"
BC_PURPLE="\033[035m"
BC_CYAN="\033[036m"
BC_WHITE="\033[037m"
BC_RESET="\033[039m"

colours=($BC_BLACK $BC_RED $BC_GREEN $BC_YELLOW $BC_BLUE $BC_CYAN $BC_WHITE)
hostindex=$(( 0x$(hostname | md5sum | head -c 1) % 7 ))
# hostindex=$(( $(hostname | od | tr -d ' \n' | head -c 10) % 7 ))
hostcolour=${colours[$hostindex]}

# prompt
case "$TERM" in
	"dumb")
		PS1="> "
		;;
	xterm*|rxvt*|eterm*\screen*)
		PS1="\[$BC_BLACK\][\t]\[$BC_RED\] \u@\[$hostcolour\]\h \[$BC_GREEN\]\w:\[$BC_RESET\] "
		;;
	*)
		PS="> "
		;;
esac

alias grep='grep -i --color=always'       # make grep case insensitive
alias latest='ltt | head -1'
alias sidediff='diff -ybB -W 180'  # diff, side-by-side, ignore whitespace, column-width 180

#set default text editor
export EDITOR=emacs

# ls family
alias ll="ls -l --group-directories-first"
alias ls='ls -hF --color'  # add colors for filetype recognition
alias la='ls -Alh'          # show hidden files
alias lx='ls -lXBh'         # sort by extension
alias lk='ls -lSrh'         # sort by size, biggest last
alias lc='ls -ltcrh'        # sort by and show change time, most recent last
alias lu='ls -lturh'        # sort by and show access time, most recent last
alias lt='ls -ltrh'         # sort by date, most recent last
alias lm='ls -ahl |more'    # pipe through 'more'
alias lr='ls -lhR'          # recursive ls
alias ltt='ls -tc --color=tty' # sort by change time, most recent first
alias tree='tree -Csu'     # nice alternative to 'recursive ls'

function extract()      # Handy Extract Program.
{
     if [ -f $1 ] ; then
	 case $1 in
	     *.tar.bz2)   tar xvjf $1     ;;
	     *.tar.gz)    tar xvzf $1     ;;
	     *.bz2)       bunzip2 $1      ;;
	     *.rar)       unrar x $1      ;;
	     *.gz)        gunzip $1       ;;
	     *.tar)       tar xvf $1      ;;
	     *.tbz2)      tar xvjf $1     ;;
	     *.tgz)       tar xvzf $1     ;;
	     *.zip)       unzip $1        ;;
	     *.Z)         uncompress $1   ;;
	     *.7z)        7z x $1         ;;
	     *)           echo "'$1' cannot be extracted via >extract<" ;;
	 esac
     else
	 echo "'$1' is not a valid file"
     fi
}

# A shortcut function that simplifies usage of xclip.
# - Accepts input from either stdin (pipe), or params.
# From Nathan Broadbent: http://madebynathan.com/2011/10/04/a-nicer-way-to-use-xclip/
# ------------------------------------------------
cb() {
  local _scs_col="\e[0;32m"; local _wrn_col='\e[1;31m'; local _trn_col='\e[0;33m'
  # Check that xclip is installed.
  if ! type xclip > /dev/null 2>&1; then
    echo -e "$_wrn_col""You must have the 'xclip' program installed.\e[0m"
  # Check user is not root (root doesn't have access to user xorg server)
  elif [[ "$USER" == "root" ]]; then
    echo -e "$_wrn_col""Must be regular user (not root) to copy a file to the clipboard.\e[0m"
  else
    # If no tty, data should be available on stdin
    if ! [[ "$( tty )" == /dev/* ]]; then
      input="$(< /dev/stdin)"
    # Else, fetch input from params
    else
      input="$*"
    fi
    if [ -z "$input" ]; then  # If no input, print usage message.
      echo "Copies a string to the clipboard."
      echo "Usage: cb <string>"
      echo "       echo <string> | cb"
    else
      # Copy input to clipboard
      echo -n "$input" | xclip -selection c
      # Truncate text for status
      if [ ${#input} -gt 80 ]; then input="$(echo $input | cut -c1-80)$_trn_col...\e[0m"; fi
      # Print status.
      echo -e "$_scs_col""Copied to clipboard:\e[0m $input"
    fi
  fi
}
# Aliases / functions leveraging the cb() function
# ------------------------------------------------
# Copy contents of a file
function cbf() { cat "$1" | cb; }
# Copy SSH public key
alias cbssh="cbf ~/.ssh/id_rsa.pub"
# Copy current working directory
alias cbwd="pwd | cb"
# Copy most recent command in bash history
alias cbhs="cat $HISTFILE | tail -n 1 | cb"

# grep() {
#     if [[ -t 1 ]]; then
# 	command grep -n -i --color=always "$@"
#     else
# 	command grep -i "$@"
#     fi
# }

#last line
