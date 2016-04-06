#!/bin/csh  -f
#-------------------------------------------------------
#
# cshrc file for Common usage
# 
# Please read all the comments below and when requested,
# make changes to reflect your system set-up.
#
# Log
# R.Miller/NWS ??
# P.Bruehl/NWS	11/95	Revised for 5.2.1, documented
# P.Bruehl/NWS	10/96	Updated for 5.4
# P.Bruehl/NWS   4/97   Updated to support Linux
# R.Rozumalski   1/00   Added Workstation Eta support
#-------------------------------------------------------
#
# Set file permission mask (-rwxr-x-r-x)
#
umask 022
#
# Define system type.
#
setenv OS 			`/bin/uname`
#
# Set the DISPLAY variable if it isn't set
#
setenv HOMEHOST ""
#
# Set your default printer (Berkeley and System 5 unix)
#
# "hp4" is just an example--add your printer name here
# and uncomment the following two lines
#
#setenv PRINTER        hp4
#setenv LPDEST         $PRINTER

#  Define the root of your data directory tree.
#  If you don't set it here, it will default to the
#  $NAWIPS/metdat
#
setenv METDAT         /usr1/nawips/metdat

#  Define the location of the NAWIPS, WS ETA, and RTDATA
#  distributions.  Note that this has changes from previous
#  versions of the Common.cshrc  file.
#
switch ($OS)

	case "HP-UX":
	case "HPUX":
		setenv NAWIPS /usr1/nawips
		setenv WS_ETA /usr1/worketa
		setenv RTDATA /usr1/nawips/gribmaster2.1
	breaksw

	case "Linux":
		setenv NAWIPS /usr1/nawips
		setenv WS_ETA /usr1/worketa
		setenv RTDATA /usr1/nawips/gribmaster2.1
	breaksw

	default:
		setenv NAWIPS /usr1/nawips
		setenv WS_ETA /usr1/worketa
		setenv RTDATA /usr1/nawips/gribmaster2.1
	breaksw

endsw

# Some standatrd vi editor defaults.  If you uncomment
# this setting, it will override your .exrc file
#
#setenv EXINIT	"set ic nowrapscan showmode|map t :'a,'b"

#
#  Make OS-specific definitions
#  Do not change these unless you're absolutely sure.
#
switch ($OS)

   case "HP-UX":
   case "HPUX":
	setenv ARCH          hp
	setenv NA_OS         hpux
	setenv ETA_OS        hpux
	setenv OS            HPUX
	setenv MANPATH "/usr/man:/usr/local/share/man:/usr/local/man:/usr/contrib/man:/usr/local/X11/man"
	setenv F77_FLAG	"+E4 +E6"
	setenv CC_FLAG	""
	setenv F77_LINK	""    
	setenv RANLIB	"touch"
   breaksw

   case "Linux":
	setenv ARCH          linux
	setenv NA_OS         linux
	setenv ETA_OS        linux
	setenv MANPATH "/usr/man:/usr/local/share/man:/usr/local/man:/usr/contrib/man"
	setenv F77_FLAG "+E4 +E6"
	setenv CC_FLAG  ""
	setenv F77_LINK ""   
	setenv RANLIB   "touch"
   breaksw

   default:
	echo ".cshrc does not know about $OS type."
   breaksw

endsw

#
#  Set up a default search path.
#  You may add directories here, but note that order is important.
#  Also, if you plan of using your compilers it is important that you
#  add them you the path below.  For HPUX users this will likely be
#  /opt/ansic/bin, /opt/fortran90/bin, and /opt/fortran/bin . For
#  LINUX users you will either use /usr/absoft/bin or $PGI/linux86/bin
#  depending on the compiler. Also you will have to set the $PGI or
#  $ABSOFT environmental vaiables somewhere above.  By default these
#  are:
#  setenv PGI     /usr/pgi
#  setenv ABSOFT  /usr/absoft

set path = ( ~/bin /usr/local/bin /usr/bin/X11 /usr/contrib/bin/X11  \
             /usr/local/X11/bin /bin  /usr/bin /usr/etc /etc .)

#
#  Source GEMPAK/NAWIPS applications. 
#
if ( -e $NAWIPS/Nawips.cshrc ) then
        source $NAWIPS/Nawips.cshrc
endif

#
#  Source Workstation ETA applications.
#
if ( -e $WS_ETA/ETA.cshrc ) then
    source $WS_ETA/ETA.cshrc
endif

#
#  X-Windows application resource search path 
#
if ($?xresources) then
        setenv XUSERFILESEARCHPATH \
        ~/.app-defaults/%N:${xresources}:/usr/local/lib/app-defaults/%N
        unset xresources
else 
        setenv XUSERFILESEARCHPATH \
        ~/.app-defaults/%N:/usr/local/lib/app-defaults/%N
endif

# Miscellaneous definitions
#
set filec
set cdpath = ( ~ )
set notify
set history = 200 savehist = 200
limit coredumpsize 0

# Done!
#
# Check for noninteractive shell
#
# No need to change this
#
set TTY="`tty`"
if ($?0 || ! $?prompt) exit 0
if ( "$TTY" == "not a tty" ) exit 0

#
#  Set the DISPLAY to use fastest transport available if
#  sitting at the console of the workstation.
#
if ( "$TTY" == "/dev/console" ) then
     setenv DISPLAY :0.0
endif

#
# Make a failsafe setting for the DISPLAY variable if not 
# a console log on.
#
if ( ! $?DISPLAY ) setenv DISPLAY  ${HOMEHOST}:0.0

#
# Set prompt
#
set host=`hostname | cut -d"." -f1`
set prompt="`whoami`@${host}-> "

