#!/bin/sh
# edit file in EMACS, fallback to others if emacs is not present

# define the possible editors
# format:  command:-option
CommandLineEditors="mg: jove: emacs:-nw vim: vi: nano:"
FrameEditors="emacsclient gnuclient"

ERROR() {
  printf 'error: '
  printf "${@}"
  printf '\n'
  exit 1
}

VERBOSE() {
  [ X"${verbose}" = X"no" ] && return 0
  printf "${@}"
  printf '\n'
}

USAGE() {
  if [ -n "${1}" ]
  then
    printf 'error: '
    printf "${@}"
    printf '\n'
  fi
  cat <<EOF
usage: ${0##*/} <options> <files>
       --help                 -h            this message
       --verbose              -v            more messages
       --no-frame             -n            use console editor
       --no-wait              -b            background the frame editor
       --desktop-number       -d            show the desktop number (1..n or 0 if not supported)
       --debug                -D            show debug information
EOF
  exit 1
}

DesktopNumber() {
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
debug=no
frame=yes
wait=yes

# parse options
while getopts :hvbdnD-: option
do
  # convert long options
  if [ X"${option}" = X"-" ]
  then
    option="${OPTARG%%=*}"
    OPTARG="${OPTARG#${option}}"
    OPTARG="${OPTARG#=}"
  fi
  case "${option}" in
    (v|verbose)
      verbose=yes
      ;;

    (b|no-wait)
      wait=no
      ;;

    (d|desktop-number)
      DesktopNumber
      exit 0
      ;;

    (n|no-frame)
      frame=no
      ;;

    (--)
      break
      ;;

    (D|debug)
      debug=yes
      ;;

    (h|help)
      USAGE
      ;;

    ('?')
      USAGE 'invalid option: -%s' "${OPTARG}"
      ;;

    (*)
      USAGE 'invalid option: --%s' "${option}"
      ;;
  esac
done

shift $((OPTIND - 1))

# verify arguments
#[ ${#} -ne 0 ] && USAGE 'extraneous arguments: %s' "${*}"
[ ${#} -eq 0 ] && USAGE 'missing arguments'

# enable debugging
[ X"${debug}" = X"yes" ] && set -x

# command line edit
for value in $CommandLineEditors
do
  cl_editor="${value%%:*}"
  cl_options="${value#*:}"
  cl_editor=$(command -v "${cl_editor}" 2> /dev/null)
  [ -x "${cl_editor}" ] && break
done
[ -z "${cl_editor}" ] && echo "warning: no backup editor"

[ X"${verbose}" = X"yes" ] && echo "selected: ${cl_editor}"

# in frame edit
frame_editor=
if [ -n "${DISPLAY}" ]
then
  for value in ${FrameEditors}
  do
    frame_editor="$(command -v "${value}" 2> /dev/null)"
    [ -x "${frame_editor}" ] && break
  done
  [ -z "${frame_editor}" ] && echo "warning: no in-frame editor"
fi

[ X"${verbose}" = X"yes" ] && echo "selected: ${frame_editor}"

if CurrentDesktop=$(DesktopNumber) && [ X"${frame}" = X"yes" ] && [ -n "${frame_editor}" ]
then
  # echo CurrentDesktop = ${CurrentDesktop}
  server_socket="/tmp/emacs$(id -u)-${CurrentDesktop}/server"
  extra_flags=
  [ X"${wait}" = X"no" ] && extra_flags="-n"
  if [ -S "${server_socket}" ]
  then
    "${frame_editor}" ${extra_flags} -s "${server_socket}" "${@}" && exit 0
    echo
  fi
fi

# default to command line editing
[ -n "${cl_editor}" ] && exec "${cl_editor}" ${cl_options} "${@}"

ERROR 'could not find a good editor'
