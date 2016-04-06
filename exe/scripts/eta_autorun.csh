#!/bin/csh 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#  ETA_AUTORUN           Version 2.0                                    #
#                                                                       #
#  The ETA_AUTORUN script is intended to be run from a cron, although   #
#  it can be run from the command line.  It is primarily for real-time  #
#  modeling and drives the three ETA_ scripts included with this WS ETA #
#  package.  There in only 1 argument to this script, CYCLE, the cycle  #
#  hour of the data you wish to use to for the initialization of the    #
#  model forecast. Everything else is specified by the user below.      #
#                                                                       #
#  Note: Before running this script you need to read and configure the  #
#  ETA_DOWNLOAD, ETA_RUN, AND ETA_CONVERT scripts.                      #
#                                                                       #
#  The only argument to this script is CYCLE, which is the cycle time   #
#  of the NCEP model run that you wish to use for your run.             #
#                                                                       #
#  LOG:                                                                 #
#       R.Rozumalski - 03/00    Version 1.0  - Initial Release          #
#       R.Rozumalski - 04/00    Added notification of model failure     #
#       R.Rozumalski - 06/00    Version 1.1                             #
#       R.Rozumalski - 04/01    Version 2.0                             #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#           ETA_AUTORUN SCRIPT CONFIGURATION SECTION                    #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
# 1. CYCLE                                                              #
#                                                                       #
#    CYCLE is the only argument that is passed to the AUTORUN_ETA       #
#    script and refers to the initialization time of the NCEP           #
#    operational run you wish to use for your local model run.          #
#    Possible choices are 00, 06, 12, and 18 UTC. Since this program    #
#    is intended for real-time model forecasts, you want to start       #
#    this script as soon as model data from a particular run are        #
#    available on the an ftp server.                                    #
#                                                                       #
#    Here is a summary of the APPROXIMATE times that these runs are     #
#    available via ftp:                                                 #
#                                                                       #
#                            GRID                                       #
#      CYCLE     104      211     221      avn                          #
#            -------------------------------------                      #
#     00 UTC | 03 UTC | 03 UTC | 03 UTC | 04 UTC |                      #
#            -------------------------------------                      #
#     06 UTC | 09 UTC | 09 UTC | 09 UTC | 10 UTC |                      #
#            -------------------------------------                      #
#     12 UTC | 15 UTC | 15 UTC | 15 UTC | 16 UTC |                      #
#            -------------------------------------                      #
#     18 UTC | 21 UTC | 21 UTC | 21 UTC | 22 UTC |                      #
#            -------------------------------------                      #
#                                                                       #
#                                                                       #
#    For example, if you want to start a real-time run from a 00 UTC    #
#    NCEP model you might add the following to your crontab file:       #
#                                                                       #
#    0 21 * * * /usr1/worketa/exe/scripts/eta_autorun.csh 00 >>         #
#               /tmp/eta_autorun00.log  2>&1  (all 1 line)              #
#                                                                       #
#    NOTE: the above example assumes Central Standard Time (CST) since  #
#          21 CST = 03 UTC.                                             #
#                                                                       #
#########################################################################
set CYCLE = $1

#########################################################################
# 2. INIT_FCST_HR, FINAL_FCST_HR, FREQ                                  #
#                                                                       #
#    INIT_FCST_HR is the initial forecast hour of a particular NCEP     #
#    model run used to initialize your real-time model run.  It is NOT  #
#    the cycle time (i.e, 00, 06, 12, or 18 UTC), but the forecast hour.#
#    This value does not have to be 0 (0-h forecast) but can be any     #
#    forecast hour available. Foe example, you can initialize your run  #
#    from a 12h NCEP forecast (INIT_FCST_HR = 12) or a 36 hour forecast #
#    (INIT_FCST_HR = 12). This is a good idea if you want to get a head #
#    start in getting your high-resolution model forecast to the users. #
#                                                                       #
#    FINAL_FCST_HR is the final hour of NCEP model data to download for #
#    use as boundary conditions in your run.                            #
#                                                                       #
#    FREQ is the frequency (in hours) of NCEP forecast data to download #
#    between the initialization and final time. THE HIGHER THE FREQUENCY#
#    THE BETTER (i,e. a lower number), with 3 hours being the highest   #
#    frequency currently available. Use 3!                              #
#                                                                       #
#    For more information see the eta_download.csh script.              #
#                                                                       #
#########################################################################
set INIT_FCST_HR  = 00
set FINAL_FCST_HR = 06
set FREQ          = 03

#########################################################################
# 3. MODELS  { tiles | 104 | 212 | 221 | avn }                          #
#                                                                       #
#    MODELS is the list of NCEP models to use for the initialization of #
#    you model run. A complete list can be found in the eta_download.csh#
#    file.  The purpose of this option is to provided an alternative    #
#    dataset should your primary choice not be available.               #
#                                                                       #
#########################################################################
set MODELS = ( 104 )

#########################################################################
# 4. SITES { REGION | NCEP | OSO }                                      #
#                                                                       #
#    SITES is a list of data ftp servers to access for the necessary    #
#    GRIB data necessary to initialize the model.  Your choices are:    #
#    REGION, NCEP, or OSO. Note that IP address for each of these       #
#    locations must be defined below.  This script will attempt to      #
#    aquire data from the first site in the list and then after a       #
#    user-defined number of times (MAX_TRY), try the next server.       #
#                                                                       #
#    NOTE: You must have a .netrc file with the login information for   #
#    each of these sites in the top level of the user "eta" account.    #
#    Also, make sure that the .netrc permissions are set to 600 .       #
#########################################################################
set SITES   = ( NCEP )
set MAX_TRY = 3

set REGION  = 105.165.6.195
set NCEP    = ftp.ncep.noaa.gov
set OSO     = 140.90.6.103

#########################################################################
# 5. FAIL_OVER   { SITE | MODEL }                                       #
#                                                                       #
#    FAIL_OVER deterines what to do in case of a failure in downloading #
#    the selected model GRIB data from the chosen ftp server.  The      #
#    options are to try a different ftp server (SITE) or model dataset  #
#    (MODEL).  The models are listed in order of preference in the      #
#    MODELS parameter above. Likewise, the server locations are listed  #
#    in the SITES variable above. If you choose SITES, the script will  #
#    try to download data from the next location in the list after      #
#    MAX_TRY attempts at the previous site.                             #
#                                                                       #
#########################################################################
set FAIL_OVER = SITE

#########################################################################
# 6. WAIT                                                               #
#                                                                       #
#    WAIT is the amount of time, in seconds, to delay before trying to  #
#    get the GRIB data from a server following a failure due to network #
#    or server problem.                                                 #
#                                                                       #
#    A good choice for WAIT is 600 (s; 10 minutes).                     #
#                                                                       #
#########################################################################
set WAIT  = 600

#########################################################################
# 7. AUTO_CONVERT  { YES | NO }                                         #
#                                                                       #
#    Set AUTO_CONVERT = yes if you want to convert the WS ETA model     #
#    data on the fly, i.e, while the model is still running.  If        #
#    AUTO_CONVERT = NO, then the model forecast data will be converted  #
#    following the end of the run.                                      #
#                                                                       #
#    If AUTO_CONVERT = YES, then eta_autorun.csh script will spawn the  #
#    eta_autoconvert.csh program that runs in the background with the   #
#    model. For more information please read the eta_autoconvert.csh    #
#    file in the $WS_ETA/exe/scripts directory.                         #
#                                                                       #
#    NOTE: If you are running on a multi-CPU system it is a good the    #
#          run will go much more quikly if AUTO_CONVERT = NO.           #
#                                                                       #
#########################################################################
set AUTO_CONVERT = YES

#########################################################################
# 8. MAIL_INFO                                                          #
#                                                                       #
#    Set MAIL_INFO = YES if you want to be notified of a model failure. #
#    If the WS Eta fails to run to completion, an email message will be #
#    sent to all those users listed in USERS providing you with details #
#    regarding the run. The users should be listed without commas (,)   #
#    but encased in quotes.                                             #
#                                                                       #
#    Note that this option assumes that you have sendmail configured on #
#    your system.                                                       #
#                                                                       #
#    If MAIL_INFO = NO then no message is sent but all log files are    #
#    still saved in the $ETA_TMP/eta_crash directory.                   #
#                                                                       #
#########################################################################
set MAIL_INFO = NO
set USERS     = "robert.rozumalski@noaa.gov roz@comet.ucar.edu"


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#    END OF PRIMARY CONFIGURATION.  THERE IS NO NEED TO CHANGE          #
#    ANYTHING BELOW THIS LINE.                                          #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
# Preliminary stuff                                                     #
#########################################################################
unalias rm
unalias cd

#  MAKE SURE THAT ETA_OS IS CORRECTLY SET
#
env | grep ETA_OS >& /dev/null
if ( $status != 0 ) then
    echo " "
    echo "WARNING:  ETA_OS is not defined."
    echo " "
    set OS = ( `uname` )
    if ( $OS == HP-UX ) then
        setenv ETA_OS hpux
    else if ( $OS == linux ) then
        setenv ETA_OS linux
    else
        echo " "
        echo "This OS ($OS) is not supported - EXIT"
        echo " "
        exit 99
    endif                                                                            
endif

#  MAKE SURE THAT THE WS_ETA ENVIRONMENTAL VARIABLE IS SET
#
env | grep WS_ETA >& /dev/null
if ( $status != 0 ) then
    echo " "
    echo "WARNING:  WS_ETA is not defined."
    echo " "
    set PD = ( `pwd | cut -d"worketa" -f1`worketa )
    setenv WS_ETA $PD
    echo "Setting WS_ETA to $WS_ETA"
    echo " "
endif

#  SET THE REMAINING ENVIRONMENTAL VARIABLES
#
if ( -e $WS_ETA/ETA.cshrc  ) then
   source $WS_ETA/ETA.cshrc
else
   echo "ERROR: $WS_ETA/ETA.cshrc is not found."
   echo "       Go find it and try again."
   echo " "
   exit 99
endif

#  CHECK THAT THE USER INPUT IS CORRECT
#
if ( $#argv < 1 ) then
   echo " "
   echo "HEY,"
   echo "    YOU FORGOT THE CYCLE TIME: eta_autorun.csh <cycle> "
   echo " "
   echo "    Where <cycle> is 00, 06, 12, or 18 (UTC)"
   echo " "
   exit 99
endif

set AUTO_CONVERT = `echo $AUTO_CONVERT | tr '[A-Z]' '[a-z]'`

set cnts = ( `echo $CYCLE | wc` )
if ( $cnts[3] < 3 ) set CYCLE = 0$CYCLE

if ( $CYCLE != 00 && $CYCLE != 06 && $CYCLE != 12 && $CYCLE != 18 ) then
   echo " "
   echo "HEY,"
   echo "    THE CYCLE TIME IS INCORRECT ($CYCLE):  eta_autorun.csh <cycle> "
   echo "    Where <cycle> is 00, 06, 12, or 18 (UTC)"
   echo " "
   exit 99
endif

  
set yy   = `date  -u +%y`
set mm   = `date  -u +%m`
set dd   = `date  -u +%d`
set hh   = $CYCLE



#  WE ARE OFF AND MODELING
#

set MAIL_INFO =  `echo $MAIL_INFO | tr '[A-Z]' '[a-z]'`

cd $ETA_RUN
if ( -e $ETA_RUN/this_run ) rm -f $ETA_RUN/this_run

set INIT    = $INIT_FCST_HR
set FINAL   = $FINAL_FCST_HR
set SUCCESS = 0
set ATTEMPT = 1
set PATTEMPT = 1
set NM      = 1
set COUNT   = 1
set NS      = 1

@ NTIMES =  10800 / $WAIT

#  FIRST, CALL THE ETA_DOWNLOAD SCRIPT TO GET THE DATA FROM NCEP
#
echo " "
while ( $SUCCESS != 2 )

   if ( $SITES[${NS}] == REGION ) then
      set FTP = R
      set MACHINE = $REGION
   else if ( $SITES[${NS}] == NCEP ) then
      set FTP = N
      set MACHINE = $NCEP
   else if ( $SITES[${NS}] == OSO ) then
      set FTP = O
      set MACHINE = $OSO
   endif

   if ( -e $ETA_TMP/eta_lock ) then

      if ( $NTIMES == $COUNT ) exit
      echo " "
      echo "WARNING:  The SOO-SAC Workstation ETA is already running."
      echo "          If you believe this is in error then delete the"
      echo "          $ETA_TMP/eta_lock file.  "
      echo " "
      echo "          Otherwise, this run will begin as soon as the previous"
      echo "          run has completed."
      echo " "
      echo "          If you want to kill the current (this) run, type:"
      echo " "
      echo "                  kill $$ "
      echo " "
      echo "          at the command line."
      echo " "
      set SUCCESS = 0
      @ COUNT = $COUNT + 1

   else
   
      echo " "
      echo "FTPing the $MODELS[${NM}] GRIB data from $SITES[${NS}] - Attempt $PATTEMPT"
      echo " "
      set machine_up = yes
      if ( $ETA_OS == hpux && $FTP != R ) then

         /usr/sbin/ping $MACHINE 256 2 >& /dev/null
         if ( $status != 0 ) then
            set machine_up = no
            set SUCCESS = 1
         endif

      else if ( $ETA_OS == linux && $FTP != R ) then

         /bin/ping -c 2 -s 256 $MACHINE >& /dev/null
         if ( $status != 0 ) then
            set machine_up = no
            set SUCCESS = 1
         endif

      endif

      if ( $machine_up == yes ) then

         $ETA_SCRIPTS/eta_download.csh $MODELS[${NM}] $CYCLE $INIT $FINAL $FREQ $FTP
         set STAT = $status

         if ( $STAT == 0 ) then
            set SUCCESS = 2
         else if ( $STAT != 77 && $STAT != 29 && $STAT != 28 )  then
            echo " "
            echo "BUMMER ALERT: PLEASE CHECK CONFIGURATION."
            echo " "
            exit 1
         else if ( $STAT == 77 ) then
             echo "  Another WS Eta run already underway - Waiting for completion. "
             echo " "
             sleep $WAIT
         else  if ( $STAT != 1 ) then

            if ( $FAIL_OVER == SITE ) then

               @ ATTEMPT++

               echo " "
               echo "  A complete set of $MODELS[${NM}] GRIB files from is not available."
               echo "  The model will begin as soon as all data have been downloaded."

               if ( $ATTEMPT > $MAX_TRY ) then

                  set ATTEMPT = 1

                  @ NS++
                  if ( $NS > $#SITES ) then
                     echo " "
                     echo "  You have exhausted your options for data servers.  There are"
                     echo "  no data to be found anywhere to initialize the model run. - EXIT"
                     echo " "
                     exit 99
                  endif

                  echo " "
                  echo "  Trying next ftp server on list: $SITES[${NS}] "
                  echo " "

               endif

            else if ( $FAIL_OVER == MODEL ) then

               @ ATTEMPT++

               echo " "
               echo "  A complete set of $MODELS[${NM}] GRIB files from is not available."
               echo "  The model will begin as soon as all data have been downloaded."

               if ( $ATTEMPT > $MAX_TRY ) then

                  set ATTEMPT = 1

                  @ NM = $NM + 1
                  if ( $NM > $#MODELS ) then
                     echo " "
                     echo "  There was a failure to download a complete set of model GRIB data"
                     echo "  within the prescribed number of attempts. Terminating model run. - EXIT"
                     echo " "
                     exit 99
                  endif

                  echo " "
                  echo "  Trying next model on list: $MODELS[${NM}] "
                  echo " "

               endif

            endif

         endif

      else

         @ PATTEMPT ++
         if ( $PATTEMPT > $MAX_TRY ) then
            set PATTEMPT = 1

            @ NS = $NS + 1
            if ( $NS > $#SITES ) then
               echo " "
               echo "  You have exhausted your options for data servers.  There are"
               echo "  no data to be found anywhere to initialize the model run. - EXIT"
               echo " "
               exit 99
            endif

            echo "  Numerous attempt to contact the server have failed. "
            echo "  Failing over to the next data server on list: $SITES[${NS}]"
            echo " "

         else

            echo "  Server $SITES[${NS}] is not responding to a ping indicating that "
            echo "  the machine is down or there is a network problem."

         endif
        
      endif

   endif
  
   if ( $SUCCESS == 1 ) then
      echo "  Waiting $WAIT seconds before trying to get GRIB data again."
      echo "  Wait started at: " `date`
      echo " "
      echo "  If you want to terminate execution of this process type:"
      echo " "
      echo "  kill $$ "
      echo " "
      echo "  on a command line."
      echo " "
      sleep $WAIT
   else if ( $SUCCESS == 0 ) then
      echo " "
      echo "  Waiting $WAIT seconds before trying to start model again."
      echo "  Wait started at: " `date`
      echo " "
      sleep $WAIT
   endif

end

#  RUN THE ETA_RUN SCRIPT TO GENERATE THE INITIAL AND BOUNDARY
#  CONDITIONS AND RUN THE MODEL.
#
echo " "
if ( $AUTO_CONVERT == yes ) then
  echo "  ETA_AUTORUN: Starting eta_run.csh script with auto convert ON"
  echo " "
  $ETA_SCRIPTS/eta_run.csh A
else
  echo "  ETA_AUTORUN: Starting eta_run.csh script with auto convert OFF"
  echo " "
  $ETA_SCRIPTS/eta_run.csh
endif

grep STOP_ETA $ETA_TMP/eta_model.log >& /dev/null
if ( $status != 0 ) then

   if ( -e $ETA_TMP/eta_lock ) rm -f $ETA_TMP/eta_lock
   echo "  THERE WAS PROBLEM WITH YOUR MODEL RUN IN ETA_RUN, BUMMER - EXIT"
   echo " "
   
   if (! -e $ETA_TMP/eta_crash) then
      mkdir $ETA_TMP/eta_crash
   endif
   if (! -e $ETA_TMP/eta_crash/${yy}${mm}${dd}${hh} ) then
      mkdir $ETA_TMP/eta_crash/${yy}${mm}${dd}${hh}
   endif


   cd $ETA_TMP

   if ( -e $ETA_TMP/eta_model.log ) then
      set line     = `tail -10  $ETA_TMP/eta_model.log | grep Timestep | tail -1`
   else
      set line = "the beginning of the run or earlier."
   endif
   set logfiles = `ls *.log ftp_errors$CYCLE ftpfile$CYCLE`

   foreach logfile ($logfiles)
      mv $logfile $ETA_TMP/eta_crash/${yy}${mm}${dd}${hh}/${logfile}
   end

   if ( $MAIL_INFO == yes ) then

       echo " "                                                            > $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "Greetings:"                                                  >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "Your WS ETA run from $CYCLE Z has met an untimely demise at:">> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "    $line"                                                   >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "If you are morbidly curious, you may investigate the cause of my">> $ETA_TMP/crash.msg
       echo "crash in an attempt to correct the problem. All of the output "  >> $ETA_TMP/crash.msg
       echo "files from this run have been saved to the following directory " >> $ETA_TMP/crash.msg
       echo "and are awaiting your perusal:"                              >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "    $ETA_TMP/eta_crash "                                     >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "In addition, the files have been renamed "                   >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "    ${yy}${mm}${dd}${hh}${mn}.filename,"                     >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "to mark the time of my termination. - enjoy"                 >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg
       echo "Your WS Eta Model"                                           >> $ETA_TMP/crash.msg
       echo " "                                                           >> $ETA_TMP/crash.msg

       if ( $ETA_OS == hpux ) then
          /usr/bin/mailx -s "Your WS ETA Model Run" -r WSETA $USERS < $ETA_TMP/crash.msg
       else 
          /usr/bin/mail -s "Your WS ETA Model Run" $USERS < $ETA_TMP/crash.msg
       endif
       rm -f $ETA_TMP/crash.msg

   endif

   exit 99

endif

#  FINALLY, RUN THE ETA_CONVERT SCRIPT
#
if ( $AUTO_CONVERT == no || $AUTO_CONVERT == NO ) then
   echo "  ETA_AUTORUN: Starting eta_convert.csh script"
   echo " "
   $ETA_SCRIPTS/eta_convert.csh
endif

#  YOU ARE DONE

echo "## eta_autorun.csh Completed - `date`"
echo " "

exit 0

