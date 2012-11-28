# .bashrc

# Start X as a under ssh-agent
alias startx='ssh-agent startx'

#Make the history work for me
#See http://www.ukuug.org/events/linux2003/papers/bash_tips/
#See also, to analyze your history
#http://www.oreillynet.com/onlamp/blog/2007/01/whats_in_your_bash_history.html
HISTFILESIZE=1000000000
HISTSIZE=1000000
shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND" 

# pretty colours
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m'              # No Color

# computers
alias fran='ssh francesca'
alias hector='ssh -CX -l phill phase2b.hector.ac.uk'
# alias gabor=ssh beid.space.warwick.ac.uk'
# alias chara='ssh chara'
# alias chris='ssh alioth.space.warwick.ac.uk'
# alias oldjames='ssh albireo.space.warwick.ac.uk'
# alias canopus='ssh canopus.space.warwick.ac.uk'
# alias don='ssh alhena.space.warwick.ac.uk'
# alias dave='ssh alya.space.warwick.ac.uk'
# alias nicky='ssh enif.space.warwick.ac.uk'
alias hpcff='ssh -CX fsggst01@hpcff.fz-juelich.de'
alias warwick='ssh -CX phrebb@enif.space.warwick.ac.uk'
alias ipp='ssh -CX pamhi@gate.rzg.mpg.de'
alias poisson='ssh -X -p 4242 peter@192.168.0.13'
alias scppoisson='scp -P 4242 peter@192.168.0.13'
alias helios='ssh phill@helios.iferc-csc.org'

# nice for scp and stuff
myhpcff='fsggst01@hpcff.fz-juelich.de' 
mywarwick='phrebb@holmes.space.warwick.ac.uk'
myipp='pamhi@gate.rzg.mpg.de'
mycrpp='http://crppsvn.epfl.ch/repos/NEMORB'
myculham='phill@fuslwe.fusion.culham.ukaea.org.uk'
myhector='phill@login.hector.ac.uk'
myhelios='phill@helios.iferc-csc.org'

#directories
#orbdir=~/nemorb/prof_trunk
orbdir=~/nemorb/peter
alias cdhome='cd ~'
alias cdorb='cd ~/orb5/'
alias cdnemo='cd $orbdir'
alias cdruns='cd ${orbdir}/runs'
alias cdsrc='cd ${orbdir}/src'
alias cdmatlab=' cd ${orbdir}/matlab'
alias cdtrunk='cd ${orbdir}/prof_trunk/'
#alias ..='cd ..'
alias cdbin='cd ~/bin/'
alias cddown='cd ~/Download/'
alias cddoc='cd ~/Documents/Documentation/'
alias cdpapers='cd ~/Documents/Papers/'
alias cdwork='cd ~/Work/'
alias cdthesis='cd ~/Work/thesis/'
alias cddrop='cd ~/Dropbox/'

# commands
alias rmtemp='\rm *.*~'            # For removing temporary files
alias mkdir='mkdir -p'             # Make parent directories as required
alias editbash='emacs ~/.bashrc'
alias bashmeup='source ~/.bashrc'
alias wc="texcount.pl -v0"         # word count
alias emacs='emacs -nw'            # make emacs start in terminal mode
alias grep='grep -i --color=always'       # make grep case insensitive
alias firefox='firefox &'	   # make firefox open in background
alias matlab='matlab -nodesktop -nosplash; stty echo' # open matlab in a terminal
alias matlabo='matlab -desktop'			   # open matlab in a window
alias freedicv='freeciv-gtk2 &'    # freeciv
alias spotify='WINEDEBUG=fixme-all wine ~/Download/Spotify\ Installer.exe &'
alias latest='ltt | head -1'
alias sidediff='diff -ybB -W 180'  # diff, side-by-side, ignore whitespace, column-width 180

# typos
alias mc='mv'			   # definitely didn't mean midnight commander. crazy bastards.

#set default text editor
export EDITOR=emacs

# #always load these modules
# if [[ ${HOSTNAME} == 'fe1' ]]; then
# module load gnu/gnu64
# module load gnu/ompi64
# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/intel/mkl/10.0.011/lib/em64t
# #cd /gpfs/space/phrebb/
# export LD_LIBRARY_PATH=/software/mathlib/gnu/lib:$LD_LIBRARY_PATH
# elif [[ ${HOSTNAME} == 'fe2' ]]; then
# module load matlab/r2009a
# else
# module load intel-fc
# module load matlab-r2009a
# module load mpich2-intel
# fi

# prompt
export PS1="\u:\w> "

# look here for scripts
export PATH=/home/peter/python/epd-7.0-2-rh5-x86_64/bin:$PATH:~/bin:/usr/local/hdf5/bin/:/usr/lib64/mpi/gcc/openmpi/bin/
export PATH=$PATH:/home/peter/ParaView-3.12.0-Linux-x86_64/
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/include:/usr/local/lib/:/usr/local/lib64/:/usr/include:/usr/lib/:/usr/lib64/:/usr/lib64/mpi/gcc/openmpi/lib64/
export PYTHONPATH=$PYTHONPATH:/home/peter/python/

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

cl() {
cd "$@" ; ls
}

#---------------------------------------------------------------------#
#                  File and string related functions                  #
#---------------------------------------------------------------------#

# Open a pdf in background
# function readpdf()
# { okular "$1" ; } 

# Coloured diff
function coldiff ()
{ sidediff $@ | colordiff | less -R; }

# Find a file with a pattern in name
function ff()
{ find . -type f -iname '*'$*'*' -ls ; }

# Find a file with pattern $1 in name and Execute $2 on it
function fe()
{ find . -type f -iname '*'$1'*' -exec "${2:-file}" {} \; ; }

# find pattern in a set of files and highlight them
function fstr()
{
   OPTIND=1
   local case=""
   local usage="fstr: find string in files.
Usage: fstr [-i] \"pattern\" [\"filename pattern\"] "
   while getopts :it opt
   do
       case "$opt" in
           i) case="-i " ;;
           *) echo "$usage"; return;;
           esac
       done
   shift $(( $OPTIND - 1 ))
   if [ "$#" -lt 1 ]; then
       echo "$usage"
       return;
       fi
   local SMSO=$(tput smso)
   local RMSO=$(tput rmso)
   find . -type f -name "${2:-*}" -print0 |
xargs -0 grep -sn ${case} "$1" 2>&- | \
   sed "s/$1/${SMSO}\0${RMSO}/gI" | more
}

# Change filenames to lowercase
function lowercase()
{
   for file ; do
       filename=${file##*/}
       case "$filename" in
           */*) dirname==${file%/*} ;;
           *) dirname=.;;
esac
       nf=$(echo $filename | tr A-Z a-z)
       newname="${dirname}/${nf}"
if [ "$nf" != "$filename" ]; then
   mv "$file" "$newname"
   echo "lowercase: $file --> $newname"
   else
   echo "lowercase: $file not changed."
   fi
done
}

# Swap 2 filenames
function swap()
{
   local TMPFILE=tmp.$$
   mv "$1" $TMPFILE
   mv "$2" "$1"
   mv $TMPFILE "$2"
}

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

#last line
