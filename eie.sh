#!/bin/sh
# edit file in EMACS, fallback to others if emacs is not present

# define the possible editors
# format:  command:-option
CommandLineEditors="mg: jove: emacs:-nw vim: vi: nano:"
FrameEditors="emacsclient gnuclient"


ERROR()
{
  printf 'error: '
  printf "$@"
  printf '\n'
  exit 1
}

USAGE()
{
  if [ -n "$1" ]
  then
    printf 'error: '
    printf "$@"
    printf '\n'
  fi
  echo usage: $(basename "$0") '<options> <files>'
  echo '       --help           -h         this message'
  echo '       --verbose        -v         more messages'
  echo '       --no-frame       -n         use console editor'
  echo '       --no-wait        -b         background the frame editor'
  echo '       --desktop-number -D         show the desktop number (1..n or 0 if not supported)'
  echo '       --debug          -d         show debug information'
  exit 1
}


DesktopNumber()
{
  local CurrentDesktop kwin
  CurrentDesktop=$(qdbus org.kde.kwin /KWin currentDesktop 2> /dev/null)

  if [ -z "${CurrentDesktop}" ]
  then
    kwin=$(dcop 'kwin*' 2> /dev/null | head -n 1)
    CurrentDesktop=$(dcop "${kwin}" KWinInterface currentDesktop 2> /dev/null)
  fi
  if [ -z "${CurrentDesktop}" ]
  then
    CurrentDesktop=$(wmctrl -d 2>&1 | awk '/^[[:digit:]]+[[:space:]]+\*[[:space:]]/{ print $1 }')
  fi
  [ -z "${CurrentDesktop}" ] && CurrentDesktop=0
  echo ${CurrentDesktop}
}


# main program

verbose=no
frame=yes
wait=yes

getopt=/usr/local/bin/getopt
[ -x "${getopt}" ] || getopt=getopt
args=$(${getopt} -o hvnbDd --long=help,verbose,no-frame,no-wait,desktop-number,debug -- "$@") ||exit 1

# replace the arguments with the parsed values
eval set -- "${args}"

while :
do
  case "$1" in
    (-v|--verbose)
      verbose=yes
      ;;

    (-n|--no-frame)
      frame=no
      ;;

    (-b|--no-wait)
      wait=no
      ;;

    (-D|--desktop-number)
      DesktopNumber
      exit 0
      shift
      ;;

    (-d|--debug)
      debug=yes
      ;;

    (--)
      shift
      break
      ;;

    (-h|--help)
      USAGE
      ;;

    (*)
      USAGE 'invalid argument: "%s"' "$1"
      ;;
  esac
  shift
done

[ $# -eq 0 ] && USAGE 'missing arguments'

[ X"${debug}" = X"yes" ] && set -x

# command line edit
editor=
for value in $CommandLineEditors
do
  cl_editor="${value%%:*}"
  cl_options="${value#*:}"
  cl_editor=$(which "${cl_editor}" 2> /dev/null)
  [ -x "${cl_editor}" ] && break
done
[ -z "${cl_editor}" ] && echo "warning: no backup editor"

# in frame edit
frame_editor=
for value in $FrameEditors
do
  frame_editor=$(which "${value}" 2> /dev/null)
  [ -x "${frame_editor}" ] && break
done
[ -z "${frame_editor}" ] && echo "warning: no in frame editor"

CurrentDesktop=$(DesktopNumber)

if [ "$?" -eq 0 -a X"${frame}" = X"yes" -a -n "${frame_editor}" ]
then
  # echo CurrentDesktop = ${CurrentDesktop}
  server_socket="/tmp/emacs$(id -u)-${CurrentDesktop}/server"
  extra_flags=
  [ X"${wait}" = X"no" ] && extra_flags="-n"
  if [ -S "${server_socket}" ]
  then
    "${frame_editor}" ${extra_flags} -s "${server_socket}" "$@"
    [ $? -eq 0 ] && exit 0
    echo
  fi
fi

# default to command line editing
[ -n "${cl_editor}" ] && exec "${cl_editor}" ${cl_options} "$@"

ERROR 'could not find a good editor'
