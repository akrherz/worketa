#!/bin/csh 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#  ETA_DOWNLOAD          Version 1.1                                    #
#                                                                       #
#  This script will download requested files from the NCEP, Regional,   #
#  or SOOSAC ftp servers and convert them into a format suitable for    #
#  processing by the workstation ETA preprocessor.  This script takes   #
#  the following arguments:                                             #
#                                                                       #
# > eta_download.csh GRIB_NUM CYCLE_HR INIT_FCST_HR FINAL_FCST_HR FREQ  #
#                                                                     ^ #
#  Where:                                                         EVENT #
#                                                                       #
#        GRIB_NUM identifies the model grid to download for the initial #
#        and boundary conditions.  The choices include:                 #
#                                                                       #
#        104  - ETA Model on a 91km domain                              #
#        212  - ETA Model on a 40km domain                              #
#        221  - ETA Model on a 32km domain                              #
#        tile - ETA Model 32km Tiled data                               #
#        avn  - Global AVN Model dataset                                #
#                                                                       #
#  CYCLE_HR is the initialization time of the ETA or AVN model run,     #
#           either 00, 06, 12, or 18 (UTC).                             #
#                                                                       #
#  INIT_FCST_HR is the first forecast hour from the selected ETA or AVN #
#               model run to download.  Note that this time does not    #
#               have to necessarily be a 00 hour forecast. You can      #
#               initialize a workstation ETA model run from a 12 or 24  #
#               hour forecast.                                          #
#                                                                       #
#  FINAL_FCST_HR is the final forecast time to download from the        #
#                selected NCEP model run. This time will determine the  #
#                maximum length of a workstation ETA forecast.          #
#                                                                       #
#  FREQ is the frequency of the ETA or AVN data to download between     #
#       the INIT_FCST_HR and FINAL_FCST_HR hours. These data are to be  #
#       used as boundary conditions for the workstation ETA forecast.   #
#       It is strongly suggested that 3 hourly data be used.            #
#                                                                       #
#  EVENT is an OPTIONAL flag indicating whether this is a REAL-         #
#        TIME (R, N or O), CASE STUDY (C), or BENCHMARK (B) run.        #     
#                                                                       #
#        Note that if no argument is passed to ETA_DOWNLOAD then the    #
#        script will default to the REAL-TIME (R, N or O) setting.      # 
#                                                                       # 
#        R:  If R is selected, then the data will be obtained from the  # 
#            REGIONAL ftp server.                                       # 
#                                                                       # 
#        N:  If N is selected, then the data will be obtained from the  # 
#            NCEP ftp server.                                           # 
#                                                                       #
#        O:  If O is selected, then the data will be obtained from the  #
#            OSO ftp server.                                            # 
#                                                                       # 
#                                                                       # 
#        C:  If C is used, then NCEP GRIB data are assumed to already   #
#            reside in the  $ETA_DATA/eta_prep directory and will be    #
#            for use by the ETA in the ETA_RUN script. You would use    #
#            this option with the archived ETA model data available     #
#            from the SOO/SAC ftp site or by contacting the SOO/SAC     #
#            coordinator.  Note that the SST and snowcover data must    #
#            also reside in the $ETA_DATA/eta_prep directory.           #
#                                                                       #
#        B:  This option will download a preselected dataset from the   #
#            SOO/SAC ftp server for testing and benchmarking the        #
#            workstation Eta. If B is passed, all other arguments will  #
#            be ignored and 24 hours of the ETA 104 grid will be        #
#            downloaded from the server.                                #
#                                                                       #
#  For example:                                                         #
#       > eta_download.csh  avn 06 06 36 03 N                           #
#                                                                       #
#  will download and process the first 30 hours of AVN forecasts from   #
#  the most recent 06 UTC model run beginning with the 6 hour and       #
#  ending with the 36 hour forecast at 3 hr intervals.                  #
#                                                                       #
#  NOTE:  This version of eta_download.csh reqires that you have a      #
#         .netrc file in the users home directory with the username and #
#         passwords for the NCEP (anonymous), OSO (anonymous), regional #
#         (optional), and the soosac (optional) ftp servers.            #
#         If you are unsure of how to set up a .netrc file, just give   #
#         me a call.                                                    #
#                                                                       #
#  LOG:                                                                 #
#       R.Rozumalski - 03/00      Version 1.0 - Initial Release         #
#       R.Rozumalski - 05/00      Added Benchmarking data               #
#       R.Rozumalski - 06/00      Added OSO and Regional server options #
#       R.Rozumalski - 07/00      Version 1.1                           #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
# 1. If ARCHIVE = yes then the downloaded GRIB data will be saved to    #
#    the ARCH_DIR. Set ARCH_DIR to whatever you want.     		#
#########################################################################
set ARCHIVE   = no
set ARCH_DIR  = $ETA_DATA/archive

#########################################################################
# 2. Set the default for the event arguments, either CASE_STUDY or      #
#    REAL_TIME. Also define the default ftp site, either NCEP (N),      #
#    OSO (O), or the regional (R) server.                               #
#########################################################################
set EVENT = REAL_TIME
set FTP   = N

#########################################################################
# 3. If you plan on downloading data from your regional server, you     #
#    must define the location and naming convention of the grib files.  #
#    Also note that the regional server IP address must be included     #
#    below in (5).                                                      # 
#                                                                       #
#    REGION_DIR is the directory on the remote site where the files are #
#    kept. Example: /metdat/raw/grib                                    #
#                                                                       #
#    REMOTE_NAME identifies the naming convention used for the GRIB     #
#    files on the regional server as either ORIGINAL or GRIBMASTER.     #
#    ORIGINAL indicates that the files on the regional server have the  #
#    same name as those on the NCEP and OSO ftp servers. GRIBMASTER     #
#    means that the files have been renamed as part of the gribmaster   #
#    downloading process.                                               #
#                                                                       #
#########################################################################
set REGION_DIR  = /ext2/sacdata/metdat/raw/grib
set REMOTE_NAME = GRIBMASTER

#########################################################################
# 4. Define tile numbers to download if using the Eta model on the TILE #
#    option. To determine which tiles you need, see $ETA_DOC/tiles.gif  #
#                                                                       #
#    NOTE: The tiles option is currently NOT available. I anticipate    #
#          that NCEP will begin creating tiles again by mid-summer.     #
#                                                                       #
#########################################################################
set TILES = ( 21 22 23 15 16 17 09 10 11 )

#########################################################################
# 5. Define the addresses of the NCEP, SOOSAC, and REGIONAL ftp servers #
#########################################################################
set NCEP   = ftp.ncep.noaa.gov
set OSO    = 140.90.6.103
set SAC    = soosac.comet.ucar.edu
set REGION = 205.165.6.195

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#    END OF PRIMARY CONFIGURATION.  THERE IS NO NEED TO CHANGE          #
#    ANYTHING BELOW THIS LINE. - I HOPE                                 #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#  Preliminary stuff                                                  
#
unalias rm
unalias cd
set exit_status = 0
echo " "

#  Define the ETA_HOME directory again, just to make sure             
#
env | grep WS_ETA >& /dev/null
if ( $status != 0 ) then
    echo WS_ETA is not defined.
    exit 99
endif

if ( -e $WS_ETA/ETA.cshrc  ) then
   source $WS_ETA/ETA.cshrc
else
   echo "ERROR: $WS_ETA/ETA.cshrc is not found."
   echo "       Go find it and try again."
   echo " "
   exit 99
endif

if ( ! -e ~/.netrc  ) then
   echo "ERROR: You will need to create a .netrc file in the users"
   echo "       home directory to use this script.  Please create"
   echo "       one and try again. - EXIT"
   echo " "
   exit 99
endif

#  TEST THE ARGUMENTS TO THE SCRIPT
#
if ( $#argv >= 5  ) then

    set grid = $1

    set VALID     =  yes
    set EVENT     =  `echo $EVENT     | tr '[a-z]' '[A-Z]'`
    set ARCHIVE   =  `echo $ARCHIVE   | tr '[A-Z]' '[a-z]'`
    if ( $#argv == 5 ) then
       set FTP    = $FTP
    else
       set FTP    =  `echo $6         | tr '[A-Z]' '[a-z]'`
    endif
    set grid      =  `echo $grid      | tr '[A-Z]' '[a-z]'`

    if ( $grid == tiles ) set grid = tile

    if ( $grid != 104 && $grid != tile && $grid != avn && $grid != 221 && $grid != 212 ) then
        echo "ERROR: Grid $grid is not supported."
        set VALID = NO
    endif

    if ( $2 != 00 && $2 != 06 && $2 != 12 && $2 != 18 ) then
       echo "ERROR: Invalid Cycle time: $2"
       set VALID = NO
    endif

    if ( $3 > $4 ) then
       echo "ERROR: Initialization time ($3) greater than final forecast time ($4)."
       set VALID = NO
    endif

    if ( $5 != 3 && $5 != 03 && $5 != 6 && $5 != 06 && $5 != 12 ) then
       echo "ERROR: Incorrect Boundary Condition frequency ($5) - Suggestion: use 3 hours."
       set VALID = NO
    endif

else
#  CHECK IF THIS IS A BENCHMARK RUN
#
   set N = 1
   while ( $N <= $#argv )
       if ( $argv[${N}] == B || $argv[${N}] == b ) then
	  set EVENT = BENCHMARK
	  set VALID = YES
          set FTP   = b
       endif	
       @ N ++
   end

   if ( $EVENT != BENCHMARK ) goto early_out

endif

#  CHECK IF THERE IS A MODEL ALREADY RUNNING
#

if ( -e  $ETA_TMP/eta_lock ) then
   echo "ERROR:  $ETA_TMP/eta_lock file found. Either there is "
   echo "another model run in progress or the $ETA_TMP/eta_lock"
   echo "needs to be deleted. - EXIT"
   echo " "
   exit 97
endif

echo " "

if ( $VALID == NO ) then

early_out:

    echo "#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
    echo "#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
    echo "#                                                                       #"
    echo "#  ETA_DOWNLOAD          Version 1.1                                    #"
    echo "#                                                                       #"
    echo "#  This script will download requested files from the NCEP, Regional,   #"
    echo "#  or SOOSAC ftp servers and convert them into a format suitable for    #"
    echo "#  processing by the workstation ETA preprocessor.  This script takes   #"
    echo "#  the following arguments:                                             #"
    echo "#                                                                       #"
    echo "# > eta_download.csh GRIB_NUM CYCLE_HR INIT_FCST_HR FINAL_FCST_HR FREQ  #"
    echo "#                                                                     ^ #"
    echo "#  Where:                                                         EVENT #"
    echo "#                                                                       #"
    echo "#        GRIB_NUM identifies the model grid to download for the initial #"
    echo "#        and boundary conditions.  The choices include:                 #"
    echo "#                                                                       #"
    echo "#        104  - ETA Model on a 91km domain                              #"
    echo "#        212  - ETA Model on a 40km domain                              #"
    echo "#        221  - ETA Model on a 32km domain                              #"
    echo "#        tile - ETA Model 32km Tiled data                               #"
    echo "#        avn  - Global AVN Model dataset                                #"
    echo "#                                                                       #"
    echo "#  CYCLE_HR is the initialization time of the ETA or AVN model run,     #"
    echo "#           either 00, 06, 12, or 18 (UTC).                             #"
    echo "#                                                                       #"
    echo "#  INIT_FCST_HR is the first forecast hour from the selected ETA or AVN #"
    echo "#               model run to download.  Note that this time does not    #"
    echo "#               have to necessarily be a 00 hour forecast. You can      #"
    echo "#               initialize a workstation ETA model run from a 12 or 24  #"
    echo "#               hour forecast.                                          #"
    echo "#                                                                       #"
    echo "#  FINAL_FCST_HR is the final forecast time to download from the        #"
    echo "#                selected NCEP model run. This time will determine the  #"
    echo "#                maximum length of a workstation ETA forecast.          #"
    echo "#                                                                       #"
    echo "#  FREQ is the frequency of the ETA or AVN data to download between     #"
    echo "#       the INIT_FCST_HR and FINAL_FCST_HR hours. These data are to be  #"
    echo "#       used as boundary conditions for the workstation ETA forecast.   #"
    echo "#       It is strongly suggested that 3 hourly data be used.            #"
    echo "#                                                                       #"
    echo "#  EVENT is an OPTIONAL flag indicating whether this is a REAL-         #"
    echo "#        TIME (R, N or O), CASE STUDY (C), or BENCHMARK (B) run.        #"
    echo "#                                                                       #"
    echo "#        Note that if no argument is passed to ETA_DOWNLOAD then the    #"
    echo "#        script will default to the REAL-TIME (R, N or O) setting.      #"
    echo "#                                                                       #"
    echo "#        R:  If R is selected, then the data will be obtained from the  #"
    echo "#            REGIONAL ftp server.                                       #"
    echo "#                                                                       #"
    echo "#        N:  If N is selected, then the data will be obtained from the  #"
    echo "#            NCEP ftp server.                                           #"
    echo "#                                                                       #"
    echo "#        O:  If O is selected, then the data will be obtained from the  #"
    echo "#            OSO ftp server.                                            #"
    echo "#                                                                       #"
    echo "#        C:  If C is used, then NCEP GRIB data are assumed to already   #"
    echo "#            reside in the   ETA_DATA/eta_prep directory and will be    #"
    echo "#            for use by the ETA in the ETA_RUN script. You would use    #"
    echo "#            this option with the archived ETA model data available     #"
    echo "#            from the SOO/SAC ftp site or by contacting the SOO/SAC     #"
    echo "#            coordinator.  Note that the SST and snowcover data must    #"
    echo "#            also reside in the  ETA_DATA/eta_prep directory.           #"
    echo "#                                                                       #"
    echo "#        B:  This option will download a preselected dataset from the   #"
    echo "#            SOO/SAC ftp server for testing and benchmarking the        #"
    echo "#            workstation eta. If B is passed, all other arguments will  #"
    echo "#            be ignored and 24 hours of the ETA 104 grid will be        #"
    echo "#            downloaded from the server.                                #"
    echo "#                                                                       #"
    echo "#  For example:                                                         #"
    echo "#       > eta_download.csh  avn 06 06 36 03 N                           #"
    echo "#                                                                       #"
    echo "#  will download and process the first 30 hours of AVN forecasts from   #"
    echo "#  the most recent 06 UTC model run beginning with the 6 hour and       #"
    echo "#  ending with the 36 hour forecast at 3 hr intervals.                  #"
    echo "#                                                                       #"
    echo "#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
    echo "#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
    echo " "
    exit 2

endif

#  STEP I - DOWNLOAD THE GRIB DATA
#
set SFC_FILES = yes

onintr clean_download

cd $ETA_RUN
echo "##############################################################"
echo "############## Let the Downloading Script Begin ##############"
echo "#### Starting ETA_DOWNLOAD - `date` ####"
echo " "

# ASSIGN EACH OF THE ARGUMENTS TO VARIABLE NAME
#

set cycle  = $2
set init   = $3
set final  = $4
set inter  = $5

switch ( $FTP )
   case r:
      set EVENT = REAL_TIME
      breaksw
   case n:
      set EVENT = REAL_TIME
      breaksw
   case o:
      set EVENT = REAL_TIME
      breaksw
   case c:
      set EVENT = CASE_STUDY
      breaksw
   case b:
      set EVENT  = BENCHMARK                                                   
      set grid   = 104
      set cycle  = 12
      set init   = 00
      set final  = 24
      set inter  = 03
      breaksw
   case default:
      echo "  ERROR:  Incorrect EVENT < R N O C B > entered."
      echo " "
      exit 98
   breaksw
endsw

if ( $SFC_FILES == yes ) then
    echo "  Download Surface files: ON"
else
    echo "  Download Surface files: OFF"
endif
echo "  This is a $EVENT run"
echo " "

#  MAKE SURE THAT EACH OF THE ARGUMENTS HAVE TWO DIGITS
#
set cnts = ( `echo $cycle | wc` )
if ( $cnts[3] < 3 ) set cycle = 0$cycle

set cnts = ( `echo $init | wc` )
if ( $cnts[3] < 3 ) set init =  0$init

set cnts = ( `echo $final | wc` )
if ( $cnts[3] < 3 ) set final = 0$final

#  SET YMD TO THE CURRENT UTC DATE
#
set ymd  = `date -u +%y%m%d`
set ymd4 = `date -u +%Y%m%d`
set yy   = `date -u +%y`
set YY   = `date -u +%Y`
set mm   = `date -u +%m`
set dd   = `date -u +%d`
set ff   = ${ymd}${cycle}

#  SET THE LOCAL DATA PATH FOR THE GRIB DATA
#
set grib_path = "${ETA_DATA}/eta_prep"
cd $grib_path

#  MAKE SURE THAT THERE ARE NO PREVIOUS FILES AROUND
#  THAT MAY MESS THINGS UP
#
if ( -e .this_run )                rm -f .this_run  
if ( -e $ETA_TMP/ftp_errors )      rm -f $ETA_TMP/ftp_errors
if ( -e $ETA_TMP/tmpfile )         rm -f $ETA_TMP/tmpfile
if ( -e $ETA_TMP/ftpfile${cycle} ) rm -f $ETA_TMP/ftpfile${cycle}

set COMPLETE = 1

if ( $EVENT == REAL_TIME || $EVENT == BENCHMARK ) then

#   DEFINE REMOTE SERVER
#
    if ( $EVENT == BENCHMARK ) then
       set remote_host = $SAC
    else
       if ( $FTP == r ) set remote_host = $REGION
       if ( $FTP == n ) set remote_host = $NCEP
       if ( $FTP == o ) set remote_host = $OSO
    endif

#   CHECK TO SEE IF THE REMOTE SERVER IS LISTED IN THE USERS .netrc FILE
#
    grep $remote_host ~/.netrc >& /dev/null
    if ( $status != 0 ) then
       echo "ERROR: You will need to add $remote_host to your .netrc file"
       echo "       Please add the information and try again. - EXIT"
       echo " "
       exit 98
    endif
#
#   DEFINE THE LOCATION OF THE GRIB FILES ON THE SERVER
#
    set grib_prefix = ""

    if ( $grid == tile ) then
        if ( $EVENT == BENCHMARK ) then
           echo " "
           echo "  Tiles are not available for the test case option."
           echo " "
           exit 96
        else
           if ( $FTP == r ) then
              set remote_host = $REGION
              set remote_data_path = $REGION_DIR
              set grib_prefix = ""
           else if ( $FTP == n ) then
              set remote_host = $NCEP
              set remote_data_path = /pub/gcp/ldas/eta.tiles
           else if ( $FTP == o ) then
              echo " "
              echo "  Tiles are not available from the OSO server."
              echo " "
              exit 96
           endif
        endif
    else if ( $grid == 212 ) then
        if ( $EVENT == BENCHMARK ) then
           set remote_data_path =  data/benchmark
        else
           if ( $FTP == r ) then
              set remote_host = $REGION
              set remote_data_path = $REGION_DIR
              set grib_prefix = icwf.${ymd}
           else if ( $FTP == n ) then
              set remote_host = $NCEP
              set remote_data_path = /pub/data1/eta/erl.$ymd
           else if ( $FTP == o ) then
              set remote_host = $OSO
              set remote_data_path = /ncep/erl/icwf.$ymd
           endif
        endif
    else if ( $grid == avn ) then
        if ( $EVENT == BENCHMARK ) then
           set remote_data_path =  data/benchmark
        else
           if ( $FTP == r ) then
              set remote_host = $REGION
              set remote_data_path = $REGION_DIR
              set grib_prefix = avn.${ymd}
           else if ( $FTP == n ) then
              set remote_host = $NCEP
              set remote_data_path = /pub/data1/avn/avn.$yy$mm$dd
           else if ( $FTP == o ) then
              set remote_host = $OSO
              set remote_data_path = /ncepb/avn/avn.$ymd
           endif
        endif
    else if ( $grid == 104 ) then
        if ( $EVENT == BENCHMARK ) then
           set remote_data_path =  data/benchmark
        else
            if ( $FTP == r ) then
              set remote_host = $REGION
              set remote_data_path = $REGION_DIR
              set grib_prefix = eta104.${ymd}
           else if ( $FTP == n ) then
              set remote_host = $NCEP
              set remote_data_path = /pub/data1/eta/erl.$ymd
           else if ( $FTP == o ) then
              set remote_host = $OSO
              set remote_data_path = /ncep/erl/eta104.$ymd
           endif
        endif
    else if ( $grid == 221 ) then
        if ( $EVENT == BENCHMARK ) then
           set remote_data_path =  data/benchmark
        else
           if ( $FTP == r ) then
              echo "   The 221 grid is not available on the regional ftp server."
              echo "   Please go to NCEP for these data."
              echo " "
              exit 96
           else if ( $FTP == n ) then
              set remote_host = $NCEP
              set remote_data_path = /pub/data1/eta/erl.$ymd
           else if ( $FTP == o ) then
              echo "   The 221 grid is not available on the OSO ftp server."
              echo "   Please go to NCEP for these data."
              echo " "
              exit 96
           endif
        endif
    else
        echo "SORRY, GRID $grid is not supported"
        echo " "
        exit 96
    endif

#   BEGIN THE PROCESS OF DETERMINING WHICH FILES TO GET
#   FROM THE SERVER.  
#
    echo "cd /home/models/iowaeta/worketa/data/eta_prep " > $ETA_TMP/ldmPUT${cycle}

    echo "prompt"                           >  $ETA_TMP/ftpfile${cycle}
    echo "bin"                              >> $ETA_TMP/ftpfile${cycle}

    echo "cd $remote_data_path"             >> $ETA_TMP/ftpfile${cycle}

    echo "  FTPing $grid Grid data from the ${cycle} UTC cycle run beginning"
    echo "  with the ${init} hr forecast and ending with the ${final} hr forecast."
    echo " "

    echo "  ##########################################################"
    if ( $EVENT == BENCHMARK ) then
       echo "  ######  Downloading files from SOOSAC ftp server  ########"
    else
       if ( $FTP == r ) then
          echo "  #####  Downloading files from REGIONAL ftp server  #######"
       else if ( $FTP == n ) then
          echo "  #######  Downloading files from NCEP ftp server  #########"
       else if ( $FTP == o ) then
          echo "  ########  Downloading files from OSO ftp server  #########"
       endif
    endif
    echo "  #### Downloading Began - `date` ####"
    echo " "
    echo "  #### Attempting to downloaded the following grib files:"
    echo " "

    set fh = ${init}
    while ( ${fh} <= ${final} )

       set cnts = ( `echo $fh | wc` )
       if ( $cnts[3] < 3 ) set fh = 0$fh

       if ( $grid == tile ) then

           foreach tile ( $TILES )
              set remote_data_file = eta.AWIP32${fh}.${tile}
        
              if ( ! -e ${remote_data_file} ) then
                 echo "cd ${remote_data_path}/${cycle}z/f${fh}" >> $ETA_TMP/ftpfile${cycle}
                 echo "get ${remote_data_file}"                 >> $ETA_TMP/ftpfile${cycle}
		 echo "/home/ldm/bin/pqinsert ${remote_data_file}" >> $ETA_TMP/ldmPUT${cycle}
                 echo "     FILE: $remote_data_file"
              else
                 echo "  Grib file ${remote_data_file} already downloaded - SKIPPING"
              endif
           end

       else

           if ( $grid == 104 ) set remote_data_file = eta.T${cycle}Z.GRBGRD${fh}.tm00
           if ( $grid == 212 ) set remote_data_file = eta.T${cycle}Z.AWIP3D${fh}.tm00
           if ( $grid == 221 ) set remote_data_file = eta.T${cycle}Z.AWIP32${fh}.tm00
           if ( $grid == avn ) set remote_data_file = gblav.T${cycle}Z.PGrbF${fh}
           if ( $FTP == r && $REMOTE_NAME == GRIBMASTER ) then
              set remote_data_file = ${grib_prefix}.${remote_data_file}
           endif
   
           if ( ! -e ${remote_data_file} ) then
              echo "get ${remote_data_file}"                >> $ETA_TMP/ftpfile${cycle}
	      echo "/home/ldm/bin/pqinsert ${remote_data_file}" >> $ETA_TMP/ldmPUT${cycle}
              echo "     FILE: $remote_data_file"
              set   COMPLETE = 0
           else
              echo "     FILE: $remote_data_file  already downloaded - SKIPPED"
           endif

       endif

       @ fh = ${fh} + ${inter}
    end

    echo " "

#   WHILE YOU ARE POUNDING THE FTP SERVER FOR DATA, IT WOULD BE PRUDENT
#   TO GET THE MOST RECENT SURFACE FILES.  IF gdas1.T00Z.sstgrb IS FOUND
#   IN THE LOCAL DIRECTORY THEN THE SURFACE WERE DOWNLOADED PREVIOUSLY
#   AND THEY DONE NEED TO BE FTP'D AGAIN.
#
    if ( $SFC_FILES == yes ) then

       if ( ! -e gdas1.T00Z.sstgrb ) then
           if ( $EVENT == BENCHMARK ) then
              echo "cd /polenta/ftp/soo/data/benchmark"   >> $ETA_TMP/ftpfile${cycle}
           else
              if ( $FTP == r ) then
                 echo "cd  ${remote_data_path}"           >> $ETA_TMP/ftpfile${cycle}
              else if ( $FTP == n ) then
                 echo "cd  /pub/gcp/sfcflds/oper/live"    >> $ETA_TMP/ftpfile${cycle}
              endif
           endif
           if ( $FTP != o ) echo "mget *.grb"             >> $ETA_TMP/ftpfile${cycle}
           if ( $EVENT == BENCHMARK ) then
               echo "cd /polenta/ftp/soo/data/benchmark"  >> $ETA_TMP/ftpfile${cycle}
           else
              if ( $FTP == r ) then
                 echo "cd  ${remote_data_path}"           >> $ETA_TMP/ftpfile${cycle}
              else if ( $FTP == n ) then
                 echo "cd  /pub/emc/wrkstn_eta/SST_data"  >> $ETA_TMP/ftpfile${cycle}
              endif
           endif
           if ( $FTP != o ) echo "mget *sst*"             >> $ETA_TMP/ftpfile${cycle}
       endif
    endif

    echo "bye" >> $ETA_TMP/ftpfile${cycle}

#   FTP DATA FROM SERVER AND CLEAN UP FILES
#
    if ( ! $COMPLETE ) then
       ftp -v $remote_host <  $ETA_TMP/ftpfile${cycle} > $ETA_TMP/ftp_errors${cycle}
    endif

#   IF FTP HAS FAILED AND THE LAST REQUESTED GRIB FILE IS NOT
#   FOUND ON THE LOCAL DIRECTORY THEN EXIT WITH A STATUS OF 29.
#   RUNNING THIS SCRIPT AGAIN WILL BEGIN THE DOWNLOAD WITH THE
#   LAST FILE NOT FOUND ON THE FTP SERVER.
#

    if ( $grid == avn ) then
       set GRIB_FILES = `grep gblav $ETA_TMP/ftpfile${cycle} | awk '{print $2}' `
    else
       set GRIB_FILES = `grep tm00  $ETA_TMP/ftpfile${cycle} | awk '{print $2}' `
    endif

    set HAVE_ALL = 1
    foreach GRIB_FILE ($GRIB_FILES)
       if ( ! -e $GRIB_FILE ) then
          set HAVE_ALL = 0
          echo " "
          echo "  ERROR: $GRIB_FILE was not available from the server."
          echo "         Try again later or modify request."
       endif
    end

    if ( ! $HAVE_ALL ) then
       echo " "
       echo "  #Downloading Not Completed - `date`#"
       echo "  ####  All Grib file not Available - Try again later  #####"
       echo "  ##########################################################"
       echo " "
       exit 29
    else
       echo " "
       echo "  ## Downloading Completed - `date` ##"
       echo "  #######  Finished getting GRIB files - CONTINUE  #########"
       echo "  ##########################################################"
       echo " "
    endif

endif

echo "/home/ldm/bin/pqinsert imssnow.grb" >> $ETA_TMP/ldmPUT${cycle}
echo "/home/ldm/bin/pqinsert snowdepth.grb" >> $ETA_TMP/ldmPUT${cycle}
echo "mv gdas1.T00Z.sstgrb gdas2.T00Z.sstgrb" >> $ETA_TMP/ldmPUT${cycle}
echo "/home/ldm/bin/pqinsert gdas2.T00Z.sstgrb" >> $ETA_TMP/ldmPUT${cycle}
echo "/home/ldm/bin/pqinsert gdas1.T00Z.sstgrb.index" >> $ETA_TMP/ldmPUT${cycle}

chmod +x $ETA_TMP/ldmPUT${cycle}
$ETA_TMP/ldmPUT${cycle}

exit

