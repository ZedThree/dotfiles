# -*- mode: shell-script -*-
# .bashrc

#Make the history work for me
#See http://www.ukuug.org/events/linux2003/papers/bash_tips/
HISTFILESIZE=1000000000
HISTSIZE=1000000
shopt -s histappend

# Require C-d twice to kill terminal
export IGNOREEOF=1

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

# Vaguely deterministic random colour for hostname in prompt
colours=($BC_BLACK $BC_RED $BC_GREEN $BC_YELLOW $BC_BLUE $BC_CYAN $BC_WHITE)
hostindex=$(( $(hostname | od | tr -d ' \n' | head -c 10) % 7 ))
hostcolour=${colours[$hostindex]}

# Get the walltime of the last command for the prompt
# Stolen from https://stackoverflow.com/a/1862762/2043465
# With improvements by Ed Higgins
function timer_start {
  timer=${timer:-$SECONDS}
}

function timer_stop {
  timer_show=$(($SECONDS - $timer))
  timer_sec=$(printf '%02d' $(($timer_show % 60)))
  timer_min=$(printf '%02d' $((($timer_show / 60) % 60)))
  timer_hr=$(printf '%02d' $(($timer_show / 3600)))
  timer_str="$timer_hr:$timer_min:$timer_sec"
  unset timer
}

function set_prompt() {
    result=$?

    timer_stop
    if  [[ $result == 0 ]]; then # Command ran successfully
        retcolor=${BC_GREEN}
        result_emoji="😊"
        time_this=1
    elif [[ $result == 130 ]]; then # Command was killed by ^C
        result_emoji="😯"
        retcolor=${BC_BLUE}
    elif [[ $result == 148 ]]; then # Command was suspended (^Z)
        result_emoji="😔"
        retcolor=${BC_CYAN}
    else # Command died for another reason
        result_emoji="😵:[$result]"
        retcolor=${BC_RED}
        time_this=1
    fi

    # If it's useful, get the time, else print "--:--:--"
    if [ $time_this ]; then
        timer_str=$timer_str
    else
        timer_str="--:--:--"
    fi

    if [[ -n "${VIRTUAL_ENV:-}" ]]; then
        virtualenv_bit='($(basename "${VIRTUAL_ENV}")) '
    else
        virtualenv_bit=
    fi

    result_timer_show="$result_emoji <$timer_str>"
    PS1="\[$BC_BLACK\][\t]\[$retcolor\][\$result_timer_show]\[$BC_RED\] \u@\[$hostcolour\]\h $virtualenv_bit\[$BC_GREEN\]\w:\[$BC_RESET\] "
    unset timer_str
    unset time_this

    history -a
}

trap 'timer_start' DEBUG

# Fancy prompt, unless connected via TRAMP
case "$TERM" in
    "dumb")
        PS1="> "
        ;;
    xterm*|rxvt*|eterm*|screen*)
        PROMPT_COMMAND="set_prompt"
        ;;
    *)
        PS="> "
        ;;
esac

# commands
alias rmtemp='\rm *.*~'            # For removing temporary files
alias mkdir='mkdir -p'             # Make parent directories as required
alias editbash='emacs ~/.bashrc'
alias bashmeup='source ~/.bashrc'
alias sidediff='diff -ybB -W 180'  # diff, side-by-side, ignore whitespace, column-width 180

# typos
alias mc='mv'              # definitely didn't mean midnight commander. crazy bastards.

#set default text editor
export EDITOR=emacsclient

# ls family
alias ll="ls -l --group-directories-first"
alias ls='ls -hFv --color  --group-directories-first'  # add colors for filetype recognition
alias la='ls -Alh'          # show hidden files
alias lx='ls -lXBh'         # sort by extension
alias lk='ls -lSrh'         # sort by size, biggest last
alias lc='ls -ltcrh'        # sort by and show change time, most recent last
alias lu='ls -lturh'        # sort by and show access time, most recent last
alias lt='ls -ltrh'         # sort by date, most recent last
alias lm='ls -ahl |more'    # pipe through 'more'
alias lr='ls -lhR'          # recursive ls
alias ltt='ls -tc --color=tty' # sort by change time, most recent first

#---------------------------------------------------------------------#
#                  File and string related functions                  #
#---------------------------------------------------------------------#

# Coloured diff
function coldiff ()
{ sidediff $@ | colordiff | less -R; }

function extract()      # Handy Extract Program.
{
     if [[ -f $1 ]] ; then
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
         *.7z)        7za x $1        ;;
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

# Show the last accessed file
function latest() {
    lastfile=$(ltt "$@" | head -1);
    echo "$@$lastfile";
}

# Export environment variables to emacs
# Use like export-emacs PATH LD_LIBRARY_PATH
# From http://emacs.stackexchange.com/a/13232/2659
function export-emacs {
    if [ "$(emacsclient -e t)" != 't' ]; then
        return 1
    fi

    for name in "${@}"; do
        value=$(eval echo \"\$${name}\")
        emacsclient -e "(setenv \"${name}\" \"${value}\")" >/dev/null
    done
}

#last line
