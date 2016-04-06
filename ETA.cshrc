#/bin/csh  -f
#-------------------------------------------------------
#
# cshrc file for the workstation ETA Model usage
# 
# Please read all the comments below and when requested,
# make changes to reflect your system set-up.
#
# Log
#	R.Rozumalski	12/99 	Created
#       R.Rozumalski    03/01   SMP Support added
#-------------------------------------------------------
#
# Note:  the environment variable $ETA_OS must be set
# to the operating system (i.e. hpux) in order for the 
# model to run.  You may set it in your Common.cshrc
# file or it will be set here.
#
if ( ! $?ETA_OS ) then
    set OS = ( `uname` )
    if ( $OS == HP-UX || $OS == HPUX ) then
       setenv ETA_OS  hpux
    else if ( $OS == linux || $OS == LINUX ) 
       setenv ETA_OS  linux
    else
       echo "This OS ($OS) is not supported for the ETA Model."
       exit 1
    endif
else
   if ( $ETA_OS != hpux &&  $ETA_OS != linux ) then
      echo "This OS ($OS) is not supported for the ETA Model."
      exit 1
   endif
endif

#
# Make sure ETA_HOME directory exists
#
if ( ! -d $WS_ETA ) then
	echo "Can not find Workstation ETA distribution--Check directory and"
	echo "modify ETA.cshrc"
        unsetenv WS_ETA
        exit
endif

#
# WS_ETA env variables.
#
# No need to change these, unless you have a good reason
#

    setenv ETA_HOME		$WS_ETA
    setenv ETA_EXE           	$ETA_HOME/exe/$ETA_OS
    setenv ETA_LIB		$ETA_HOME/lib/$ETA_OS
    setenv ETA_RUN              $ETA_HOME/run_time
    setenv ETA_DOC		$ETA_HOME/docs
    setenv ETA_DATA		$ETA_HOME/data
    setenv ETA_LOGS		$ETA_HOME/logs
    setenv ETA_PREP             $ETA_DATA/eta_prep
    setenv ETA_SCRIPTS          $ETA_HOME/exe/scripts

    setenv LD_LIBRARY_PATH	$ETA_HOME/lib/shared

    setenv NCPUS  2
    setenv MPSTKZ 32M

# CHANGE THIS IF NECESSARY

    setenv ETA_TMP              $WS_ETA/logs

#
#  Add WS ETA executables & scripts to the end of the existing path
#
set path = ( $path $ETA_RUN $WS_ETA/exe/${ETA_OS} $WS_ETA/exe/scripts )

# Done!


