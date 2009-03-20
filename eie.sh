#!/bin/sh

# format:  command:-option
CommandLineEditors="jove: emacs:-nw vim: vi: nano:"
FrameEditors="emacsclient gnuclient"

nf=0
nw=0
while :
do
  case "$1" in
    --no-frame)
      nf=1
      shift
      ;;
    --no-wait)
      nw=1
      shift
      ;;
    *)
      break
      ;;
  esac
done


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

CurrentDesktop=$(qdbus org.kde.kwin /KWin currentDesktop 2> /dev/null)

if [ -z "${CurrentDesktop=}" ]
then
  kwin=$(dcop 'kwin*' | head -n 1)
  CurrentDesktop=$(dcop "${kwin}" KWinInterface currentDesktop 2> /dev/null)
  #echo kwin = ${kwin}
fi
#echo dt = ${CurrentDesktop}

if [ "$?" -eq 0 -a "${nf}" -eq 0 -a -n "${frame_editor}" ]
then
  # echo CurrentDesktop = ${CurrentDesktop}
  server_socket="/tmp/emacs$(id -u)-${CurrentDesktop}/server"
  extra_flags=
  [ "${nw}" -eq 1 ] && extra_flags="-n"
  if [ -S "${server_socket}" ]
  then
    "${frame_editor}" ${extra_flags} -s "${server_socket}" "$@"
    [ $? -eq 0 ] && exit 0
    echo
  fi
fi

# default to command line editing
[ -n "${cl_editor}" ] && exec "${cl_editor}" ${cl_options} "$@"

echo "error: could not find a good editor"
exit 1
