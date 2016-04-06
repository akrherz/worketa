#!/bin/csh 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#  ETA_DOWNLOAD          Version 2.0                                    #
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
#        C:  C is the case study option. If C is used, then the         #
#            necessary grib and surface files are assumed to already    #
#            reside in the $ETA_PREP directory. You can use this        #
#            option with the archived ETA model data available from the #
#            SOO/STRC ftp site and the SOO/STRC coordinator.            #
#            Note that the SST and snowcover data must also reside in   #
#            the $ETA_DATA/eta_prep directory.                          #
#                                                                       #
#        B:  This option will download a preselected dataset from the   #
#            SOO/STRC ftp server for testing and benchmarking the       #
#            workstation Eta. If B is passed, all other arguments will  #
#            be ignored and 24 hours of the 32km tiled grid will be     #
#            downloaded from the server.                                #
#                                                                       #
#  For example:                                                         #
#       > eta_download.csh  avn 06 06 36 03 N                           # 
#                                                                       #
#  will download and process the first 30 hours of AVN forecasts from   #
#  the most recent 06 UTC model run beginning with the 6 hour and       #
#  ending with the 36 hour forecast at 3 hr intervals.                  #
#                                                                       #
#  Or for the benchmark case:                                           #
#       > eta_download.csh B                                            #
#                                                                       #
#  will download and process all data needed for the WS Eta benchmark   #
#  case.                                                                #
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
#       R.Rozumalski - 04/01      Version 2.0                           #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
# 1. If ARCHIVE = yes then the downloaded GRIB data will be saved to    #
#    the ARCH_DIR. Set ARCH_DIR to whatever you want. If $ARCH_DIR does #
#    not exist then an attempt will be made to create the directory. If #
#    the directory can't be created the script will continue and the    #
#    data will be lost.                                                 #
#########################################################################
set ARCHIVE   = yes
set ARCH_DIR  = $ETA_DATA/archive

#########################################################################
# 2. If you plan on downloading data from your regional server, you     #
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
# 3. Define tile numbers to download if using the Eta model on the TILE #
#    option. To determine which tiles you need, see $ETA_DOC/tiles.gif  #
#    If you are unsure whether a tile is necessary for your model runs, #
#    then go ahead and include the tile number below.                   #
#                                                                       #
#########################################################################
set TILES = ( 09 10 15 16 )

#########################################################################
# 4. Define the addresses of the NCEP, SOOSAC, and REGIONAL ftp servers #
#########################################################################
set NCEP   = ftp.ncep.noaa.gov
set OSO    = 140.90.6.103
set SAC    = soosac.comet.ucar.edu
set REGION = your.regional.server.ip

#########################################################################
# 5. Determine which SST dataset you plan on using (HIRES or LOWRES)    #
#    The HIRES dataset in 0.5 degree resolution while the LOWRES uses   #
#    the older 1.0 degree data.                                         #
#                                                                       #
#########################################################################
set SST = HIRES

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#    END OF PRIMARY CONFIGURATION.  THERE IS NO NEED TO CHANGE          #
#    ANYTHING BELOW THIS LINE. - I HOPE                                 #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
#    Set the default for the event arguments, either CASE_STUDY or      #
#    REAL_TIME. Also define the default ftp site, either NCEP (N),      #
#    OSO (O), or the regional (R) server. Note that these values        #
#    will be overwritten by the arguments passed to the script.         #
#########################################################################
set EVENT = REAL_TIME
set FTP   = N

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
   echo "ERROR: You will need to create a .netrc file in the eta users"
   echo "       home directory to use this script.  Please create one"
   echo "       and try again. - EXIT"
   echo " "
   exit 99
endif

########################################################################
#                                                                      #
#  TEST THE ARGUMENTS TO THE SCRIPT                                    #
#  Make sure that the arguments passed by the user are valid. If not,  #
#  then bail out and print the help file.                              #
#                                                                      #
########################################################################
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
       echo "ERROR: Incorrect Boundary Condition frequency ($5) - Suggestion: use 03 hrs."
       echo " "
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

   if ( $EVENT != BENCHMARK ) set VALID = NO

endif

if ( $VALID == NO ) goto early_out


#  CHECK TO SEE IF A eta_lock FILE EXISTS, WHICH USUSALLY MEANS THAT
#  THERE IS ANOTHER MODEL RUNNING.  HOWEVER, SOMETIMES THIS FILE REMAINS
#  FOLLOWING A MODEL CRASH AND THE MODEL WON'T START UNTIL IT IS REMOVED.
#  IF THE FILE IS MORE THAN 1 DAY OLD, DELETE IT.
#  

find $ETA_TMP -name eta_lock -atime +0 -exec rm -f {} \; >& /dev/null
if ( -e  $ETA_TMP/eta_lock ) then
   echo "STOP:  $ETA_TMP/eta_lock file found. Either there is another"
   echo "       model run in progress or the $ETA_TMP/eta_lock"
   echo "       needs to be deleted. - EXIT"
   echo " "
   exit 97
endif

echo " "


####################################################################
#                                                                  #
#  STEP I - DOWNLOAD THE GRIB DATA                                 #
#                                                                  #
####################################################################
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
      set SST    = HIRES
      set grid   = tile
      set TILES  = ( 16 )
      set cycle  = 12
      set init   = 00
      set final  = 24
      set inter  = 03
   breaksw
   default:
      echo "  ERROR:  Incorrect EVENT < $FTP > entered."
      echo " "
      echo "          > eta_download.csh help "
      echo "            for more information"
      echo " "
      exit 98
   breaksw
endsw

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
set dc   = `date    +%d`
set ff   = ${ymd}${cycle}

if ( $FTP != b ) then
   set YYMMDDHH = $ff
else
   set YYMMDDHH = 01041112
endif

#  SET THE LOCAL DATA PATH FOR THE GRIB DATA
#
set grib_path = ${ETA_PREP}
cd $grib_path

#  MAKE SURE THAT THERE ARE NO PREVIOUS FILES AROUND
#  THAT MAY MESS THINGS UP
#
if ( -e $ETA_TMP/ftp_errors )      rm -f $ETA_TMP/ftp_errors
if ( -e $ETA_TMP/tmpfile )         rm -f $ETA_TMP/tmpfile
if ( -e $ETA_TMP/ftpfile${cycle} ) rm -f $ETA_TMP/ftpfile${cycle}
if ( -e $ETA_TMP/ftpsfc${cycle} )  rm -f $ETA_TMP/ftpsfc${cycle}
if ( -e $ETA_TMP/ftpgrib${cycle} ) rm -f $ETA_TMP/ftpgrib${cycle}

set COMPLETE    = 1
set grib_prefix = ""

if ( $EVENT == REAL_TIME || $EVENT == BENCHMARK ) then

#   BEGIN BY CLEANING OUT ANY/ALL FILES IN THE $ETA_PREP DIRECTORY
#   OLDER THAN 1 DAY.
#
    /usr/bin/find $ETA_PREP -type f -mtime +1 -exec rm -f {} \; >& /dev/null

#   DEFINE REMOTE SERVERS
#
    if ( $EVENT == BENCHMARK ) then
       set remote_host = $SAC
    else if ( $FTP == r ) then
       set remote_host = $REGION
    else if ( $FTP == n ) then
       set remote_host = $NCEP
    else if ( $FTP == o ) then
       set remote_host = $OSO
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
#   DEFINE THE LOCATION OF THE GRIB FILES ON THE REMOTE SERVERS
#
    if ( $grid == tile ) then
        if ( $EVENT == BENCHMARK ) then
           set remote_data_path =  data/benchmark
        else
           if ( $FTP == r ) then
              set remote_host = $REGION
              set remote_data_path = $REGION_DIR
              set grib_prefix = ""
           else if ( $FTP == n ) then
              set remote_host = $NCEP
              set remote_data_path = /pub/data1/eta/erl.${ymd}/tiles.t${cycle}z
           else if ( $FTP == o ) then
              echo " "
              echo "   Tiles are not available from the OSO server."
              echo "   Please go to NCEP for these data."
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

############################################################################
#   BEGIN THE PROCESS OF DETERMINING WHICH FILES TO GET
#   FROM THE SERVER.  IF YOU ARE DOWNLOADING DATA FROM A REGIONAL
#   SERVER IF MAY BE NECESSARY TO UNCOMMENT THE "PASSIVE" LINE BELOW.
############################################################################
#
    echo "prompt off"                        >  $ETA_TMP/ftpgrib${cycle}
    echo "bin"                              >> $ETA_TMP/ftpgrib${cycle}

#   echo "passive on"                       >> $ETA_TMP/ftpgrib${cycle}

    echo "cd $remote_data_path"             >> $ETA_TMP/ftpgrib${cycle}

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

    if ( -e $ETA_TMP/.gribfiles ) rm -f  $ETA_TMP/.gribfiles
    while ( ${fh} <= ${final} )

       set cnts = ( `echo $fh | wc` )
       if ( $cnts[3] < 3 ) set fh = 0$fh

       if ( $grid == tile ) then

           foreach tile ( $TILES )

              set cnts = ( `echo $tile | wc` )
              if ( $cnts[3] < 3 ) set tile = 0$tile

              set remote_data_file = eta.t${cycle}z.awip32${fh}.${tile}

              echo "${remote_data_file}"                 >> $ETA_TMP/.gribfiles
        
              if ( ! -e ${remote_data_file} ) then
                 echo "get ${remote_data_file}"          >> $ETA_TMP/ftpgrib${cycle}
                 echo "     FILE: $remote_data_file"
                 set   COMPLETE = 0
              else
                 echo "     Grib file ${remote_data_file} downloaded - SKIPPING"
              endif
           end

       else

           if ( $grid == 104 ) set remote_data_file = eta.T${cycle}Z.GRBGRD${fh}.tm00
           if ( $grid == 212 ) set remote_data_file = eta.T${cycle}Z.AWIP3D${fh}.tm00
           if ( $grid == 221 ) set remote_data_file = eta.T${cycle}Z.AWIP32${fh}.tm00
           if ( $grid == avn ) set remote_data_file = gblav.T${cycle}Z.PGrbF${fh}
   
           echo "${remote_data_file}"                       >> $ETA_TMP/.gribfiles
           if ( ! -e ${remote_data_file} ) then

              if ( $FTP == r && $REMOTE_NAME == GRIBMASTER ) then
                 echo "get ${grib_prefix}.${remote_data_file} ${remote_data_file}" >> $ETA_TMP/ftpgrib${cycle}
              else
                 echo "get ${remote_data_file}"             >> $ETA_TMP/ftpgrib${cycle}
              endif

              echo "     FILE: $remote_data_file"
              set   COMPLETE = 0
           else
              echo "     FILE: $remote_data_file  downloaded - SKIPPED"
           endif

       endif

       @ fh = ${fh} + ${inter}
    end

    echo " "


#   WHILE YOU ARE POUNDING THE FTP SERVER FOR DATA, IT WOULD BE PRUDENT
#   TO GET THE MOST RECENT SURFACE FILES.  
#
#   FIRST CHECK TO SEE IF THE DATA ARE PRESENT ON THE LOCAL SYSTEM. 
#   NOTE THAT SFC FIELDS ARE NOT AVAILABLE FROM OSO SERVER. YOU STILL
#   MUST DOWNLOAD THESE FILE FROM NCEP. 
#
    
    set SFC_CK = 1
    if ( $SST == HIRES  && ! -e $ETA_PREP/rtg_sst_grb_0.5   ) set SFC_CK = 0
    if ( $SST == LOWRES && ! -e $ETA_PREP/gdas1.T00Z.sstgrb ) set SFC_CK = 0
       
    if ( ! -e $ETA_PREP/imssnow.grb   )           set SFC_CK = 0
    if ( ! -e $ETA_PREP/snowdepth.grb )           set SFC_CK = 0

    if ( ! $SFC_CK ) then

       if ( $FTP == o ) echo "prompt off"              > $ETA_TMP/ftpsfc${cycle}
       if ( $FTP == o ) echo "binary"                 >> $ETA_TMP/ftpsfc${cycle}
#      if ( $FTP == o ) echo "passive on"             >> $ETA_TMP/ftpsfc${cycle}

       if ( $EVENT != BENCHMARK ) then
           if ( $FTP == r ) then
              echo "cd  ${remote_data_path}"          >> $ETA_TMP/ftpsfc${cycle}
           else if ( $FTP == n || $FTP == o ) then
              echo "cd  /pub/gcp/sfcflds/oper/live"   >> $ETA_TMP/ftpsfc${cycle}
           endif
       endif

       echo "mget *.grb"                              >> $ETA_TMP/ftpsfc${cycle}

       if ( $EVENT != BENCHMARK ) then
          if ( $FTP == r ) then
             echo "cd  ${remote_data_path}"           >> $ETA_TMP/ftpsfc${cycle}
             echo "get rtg_sst_grb_0.5"               >> $ETA_TMP/ftpsfc${cycle}
          else if ( $FTP == n || $FTP == o ) then
             if ( $SST == HIRES ) then
                set YYYYMMDD = ( `$ETA_EXE/datem1 ${YY}${mm}${dd}00 | cut -c1-8` )
                echo "cd  /pub/data1/rtg_sst/rtg_sst.$YYYYMMDD"       >> $ETA_TMP/ftpsfc${cycle}
                echo "cd  /pub/data1/rtg_sst/rtg_sst.${YY}${mm}${dd}" >> $ETA_TMP/ftpsfc${cycle}
                echo "get rtg_sst_grb_0.5"                            >> $ETA_TMP/ftpsfc${cycle}
             else if ( $SST == LOWRES ) then
                echo "cd  /pub/emc/wrkstn_eta/SST_data"               >> $ETA_TMP/ftpsfc${cycle}
                echo "get gdas1.T00Z.sstgrb"                          >> $ETA_TMP/ftpsfc${cycle}
             endif
          endif

       else

          echo "get rtg_sst_grb_0.5"                >> $ETA_TMP/ftpsfc${cycle}

       endif

    endif


#   FTP DATA FROM SERVER AND CLEAN UP FILES
#

    if ( $FTP != o ) then

       if ( -e $ETA_TMP/ftpgrib${cycle} && -e $ETA_TMP/ftpsfc${cycle} ) then
          cat $ETA_TMP/ftpgrib${cycle} $ETA_TMP/ftpsfc${cycle} > $ETA_TMP/ftpfile${cycle} 
       else if ( -e $ETA_TMP/ftpgrib${cycle} ) then
          cat $ETA_TMP/ftpgrib${cycle}                         > $ETA_TMP/ftpfile${cycle}
       else if (  $ETA_TMP/ftpsfc${cycle} ) then
          cat $ETA_TMP/ftpsfc${cycle}                          > $ETA_TMP/ftpfile${cycle}
       endif
       rm -f $ETA_TMP/ftpgrib${cycle} $ETA_TMP/ftpsfc${cycle}

    else

       cat $ETA_TMP/ftpgrib${cycle}                            > $ETA_TMP/ftpfile${cycle}
       echo "bye"                                             >> $ETA_TMP/ftpsfc${cycle}
       rm -f $ETA_TMP/ftpgrib${cycle}

    endif

    echo "bye"                                                >> $ETA_TMP/ftpfile${cycle}

    if ( ! $COMPLETE || ! $SFC_CK ) then
       ftp -v $remote_host <  $ETA_TMP/ftpfile${cycle} > $ETA_TMP/ftp_errors${cycle}
       if ( $FTP == o && ! $SFC_CK ) then
          echo "  ###  Downloading surface files from NCEP ftp server  #####"
          echo " "
          ftp -v $NCEP < $ETA_TMP/ftpsfc${cycle} >> $ETA_TMP/ftp_errors${cycle}
       endif
    endif

#################################################################################
#   IF FTP HAS FAILED AND THE LAST REQUESTED GRIB FILE IS NOT
#   FOUND ON THE LOCAL DIRECTORY THEN EXIT WITH A STATUS OF 29.
#   RUNNING THIS SCRIPT AGAIN WILL BEGIN THE DOWNLOAD WITH THE
#   LAST FILE NOT FOUND ON THE FTP SERVER.
#################################################################################
#
    set GRIB_FILES = `cat $ETA_TMP/.gribfiles`

    set HAVE_ALL = 1
    foreach GRIB_FILE ($GRIB_FILES)
       if ( ! -e $GRIB_FILE ) then
          set HAVE_ALL = 0
          echo "     RUN INTERUPTUS: $GRIB_FILE was not available."
       endif
    end

    if ( $SST == HIRES && ! -e rtg_sst_grb_0.5 ) then
        set HAVE_ALL = 0
        echo "     RUN INTERUPTUS: rtg_sst_grb_0.5 was not available."
    endif

    if ( $SST == LOWRES && ! -e gdas1.T00Z.sstgrb ) then
        set HAVE_ALL = 0
        echo "     RUN INTERUPTUS: gdas1.T00Z.sstgrb was not available."
    endif

    if ( ! -e snowdepth.grb ) then
        set HAVE_ALL = 0
        echo "     RUN INTERUPTUS: snowdepth.grb was not available."
    endif

    if ( ! -e imssnow.grb ) then
        set HAVE_ALL = 0
        echo "     RUN INTERUPTUS: imssnow.grb was not available."
    endif

    if ( -e $ETA_TMP/.gribfiles ) rm -f $ETA_TMP/.gribfiles

    if ( ! $HAVE_ALL ) then
       echo " "
       echo "     Try again later or modify request."
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



#  STEP II - PROCESS THE GRIB DATA
#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#  AFTER ALL THE REQUESTED GRIB FILES HAVE BEEN DOWNLOADED, BEGIN THE   #
#  PROCESSING OF THESE DATA INTO A FORMAT SUITABLE FOR THE WORKSTATION  #
#  ETA.  THE FILES ARE FIRST DEGRIBBED AND THEN CONVERTED INTO AN       #
#  ETA_READY FORMAT.                                                    #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#
echo "  All requested grib files are downloaded."
echo " "
echo "  ##########################################################"
echo "  ###########  Beginning GRIB data Processing  #############"
echo " "

onintr clean_convert

if ( $grid == 104 ) then
   set GRIBEXE = $ETA_EXE/dgeta2model.exe
else if ( $grid == 221 ) then
   set GRIBEXE = $ETA_EXE/dgeta2model.exe
else if ( $grid == 212 ) then
   set GRIBEXE = $ETA_EXE/dgeta2model.exe
else if ( $grid == tile ) then
   set GRIBEXE = $ETA_EXE/dgeta2model_221_tile.exe
else if ( $grid == avn ) then
   set GRIBEXE = $ETA_EXE/dgavn2model.exe
endif

if ( ! -e $GRIBEXE ) then
    echo " "
    echo "ERROR:  $GRIBEXE was not found."
    echo "        Go find it and try again. "
    echo " "
    echo "EXIT at `date`."
    echo " "
    exit 95
endif

#  DEGRIB EACH FILE.
#
#  AT THIS POINT THE PROCESSING OF THE GRIB DATA TAKES PLACE. NOTE
#  THAT A LITTLE EXTRA CODE IS NEEDED TO HANDLE THE TILED DATASET
#  SINCE EACH FORECAST TIME IS SPLIT UP INTO MULTIPLE FILES.
#
#  IF ARCHIVE = YES THEN WE WILL SAVE RAW GRIB FILES; OTHERWISE
#  DELETE THEM.
#
echo " "
echo "  Processing the GRIB files into Eta-readable data..."
echo " "

if ( $grid == tile ) then

   if ( -e $ETA_TMP/tiles.log ) rm -f $ETA_TMP/tiles.log >& /dev/null

   set fh = ${init}
   while ( ${fh} <= ${final} )

       set cnts = ( `echo $fh | wc` )
       if ( $cnts[3] < 3 ) set fh = 0$fh

       echo "./"     > $ETA_TMP/tmpfile

       foreach tile ( $TILES )
          set cnts = ( `echo $tile | wc` )
          if ( $cnts[3] < 3 ) set tile = 0$tile

          echo "eta.t${cycle}z.awip32${fh}.${tile}" >> $ETA_TMP/tmpfile
       end 

       $GRIBEXE  < $ETA_TMP/tmpfile >>&  $ETA_TMP/tiles.log

#  Comment out to keep log files
#
       rm -f $ETA_TMP/tmpfile unpkls.dat >& /dev/null
#      rm -f $ETA_TMP/tiles.log          >& /dev/null

       @ fh = ${fh} + ${inter}
   end

else

   foreach GRIB ( $GRIB_FILES )
      if ( -e $ETA_TMP/${GRIB}.log ) rm -f $ETA_TMP/${GRIB}.log
      echo "      Processing:" $GRIB
      echo $GRIB    >  $ETA_TMP/tmpfile
      echo "./"     >> $ETA_TMP/tmpfile
   
      $GRIBEXE  < $ETA_TMP/tmpfile >&  $ETA_TMP/${GRIB}.log
      rm -f $ETA_TMP/tmpfile unpkls.dat >& /dev/null

#  Comment out to keep log files
#
      rm -f $ETA_TMP/${GRIB}.log
   end
endif

if ( $ARCHIVE == yes ) then
   if ( ! -e $ARCH_DIR ) mkdir $ARCH_DIR
   mv -f $GRIB_FILES $ARCH_DIR
else
   rm -f $GRIB_FILES >& /dev/null
endif


############################################################
#  GENERATE THE "this_run" FILE.  THIS FILE IS USED
#  DETERMINE WHAT DATA TO USE FOR THE MODEL RUN. IF THE
#  FILE IS MISSING THE RUN SHOULD FAIL.
############################################################
#
if ( -e $ETA_RUN/this_run ) rm -f $ETA_RUN/this_run >& /dev/null

# FIGURE OUT WHICH FILES WERE JUST WRITTEN OUT DURING
# THE PROCESSING OF THE GRIB DATA. THEY SHOULD BE THE MOST
# RECENT DATA IN THE $ETA_PREP FILE.
#

set FILE = ( `ls -t ${YYMMDDHH}0${init}.ETA* | head -1` )
set EXT  = ( `echo $FILE | cut -d. -f2` )
set DATE = ( `echo $FILE | cut -c1-8` )

set fh = ${init}

while ( ${fh} <= ${final} )

    set cnts = ( `echo $fh | wc` )
    if ( $cnts[3] < 4 ) set fh = 0$fh
    if ( $cnts[3] < 3 ) set fh = 0$fh

    if ( -e ${DATE}${fh}.${EXT} ) then
       echo "${DATE}${fh}.${EXT}" >> $ETA_RUN/this_run
       set exit_status = 0
    else
       if ( ${fh} != ${init} ) then
          echo "File ${DATE}${fh}.${EXT} was missing for some unknown reason."
          @ fh = ${fh} - ${inter}
          echo "The run will stop at the ${fh} hour mark."
          echo " "
          exit 28
       else
          echo "The initialization file, ${DATE}${fh}.${EXT}, was missing."
          echo "This run will be terminated."
          echo " "
          exit 1
       endif
         
    endif

    @ fh = ${fh} + ${inter}
end

#if ( -f $ETA_TMP/ftpfile${cycle} )  rm -f $ETA_TMP/ftpfile${cycle}

#  RETURN TO RUN_TIME DIRECTORY
# 

if ( -e $ETA_TMP/ftpsfc${cycle}  )    rm -f $ETA_TMP/ftpsfc${cycle}
if ( -e $ETA_TMP/ftpgrib${cycle} )    rm -f $ETA_TMP/ftpgrib${cycle}

cd $ETA_RUN


echo " "
echo "  # GRIB processing completed - `date` #"
echo "  ############ GRIB data processing Completed ################"
echo "  ############################################################"
echo " "
echo " "
exit $exit_status

#  IF THE SCRIPT WAS INTERRUPTED DURING THE DOWNLOAD
#
clean_download:

    echo " "
    echo "DOWNLOAD INTERUPTED\!"
    ls *.grb >& /dev/null
    if ( $status == 0 ) rm -f imssnow.grb snowdepth.grb
    if ( -e $ETA_TMP/ftpfile${cycle} )    rm -f $ETA_TMP/ftpfile${cycle}
    if ( -e $ETA_TMP/ftp_errors${cycle} ) rm -f $ETA_TMP/ftp_errors${cycle}
    if ( -e $ETA_TMP/ftpsfc${cycle} )     rm -f $ETA_TMP/ftpsfc${cycle}
    if ( -e $ETA_TMP/ftpgrib${cycle} )    rm -f $ETA_TMP/ftpgrib${cycle}

    if ( $grid == avn ) then
       ls gblav.* >& /dev/null
       if ( $status == 0 ) set GRIBS = `ls gblav.*`
    else
       ls eta.* >& /dev/null
       if ( $status == 0 ) set GRIBS = `ls eta.*`
    endif

    if ( ! $?GRIBS ) exit 1

    if ( $#GRIBS > 0 ) then
       echo " "
       echo "Deleting the last partially downloaded GRIB: $GRIBS[$#GRIBS]"
       echo "You will have to manually inspect the remaining grib files"
       echo "in $ETA_PREP "
       echo "to determine whether they are complete. If not, delete them."
       echo " "
       rm -f $GRIBS[$#GRIBS]
       echo " "
    endif
    exit 1

#  IF THE SCRIPT WAS INTERRUPTED DURING THE CONVERSION PROCESSES
#  DO SOME CLEANUP.
#
clean_convert:
    rm -f unpkls.dat

    ls -t ???????${init}.ETA* >& /dev/null
    if ( $status == 0 ) then
       set FILES = `ls -t ???????${init}.ETA*`
       if ( $#FILES > 0 ) then
          echo "Deleting files: $FILES"
          echo " "
          rm -f $FILES
       endif
    endif
    exit 1

#  IF THERE WAS A PROBLEM WITH THE ARGUMENTS PASSED TO THE SCRIPT
#
early_out:

    echo "#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
    echo "#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
    echo "#                                                                       #"
    echo "#  ETA_DOWNLOAD          Version 2.0                                    #"
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
    echo "#        C:  C is the case study option. If C is used, then the         #"
    echo "#            necessary grib and surface files are assumed to already    #"
    echo "#            reside in the ETA_PREP directory. You can use this         #"
    echo "#            option with the archived ETA model data available from the #"
    echo "#            SOO/STRC ftp site and the SOO/STRC coordinator.            #"
    echo "#            Note that the SST and snowcover data must also reside in   #"
    echo "#            the ETA_PREP directory.                                    #"
    echo "#                                                                       #"
    echo "#        B:  This option will download a preselected dataset from the   #"
    echo "#            SOO/STRC ftp server for testing and benchmarking the       #"
    echo "#            workstation Eta. If B is passed, all other arguments will  #"
    echo "#            be ignored and 24 hours of the 32km tiled grid will be     #"
    echo "#            downloaded from the server.                                #"
    echo "#                                                                       #"
    echo "#  For example:                                                         #"
    echo "#       > eta_download.csh  avn 06 06 36 03 N                           #"
    echo "#                                                                       #"
    echo "#  will download and process the first 30 hours of AVN forecasts from   #"
    echo "#  the most recent 06 UTC model run beginning with the 6 hour and       #"
    echo "#  ending with the 36 hour forecast at 3 hr intervals.                  #"
    echo "#                                                                       #"
    echo "#  Or for the benchmark case:                                           #"
    echo "#       > eta_download.csh B                                            #"
    echo "#                                                                       #"
    echo "#  will download and process all data needed for the WS Eta benchmark   #"
    echo "#  case.                                                                #"
    echo "#                                                                       #"
    echo "#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
    echo "#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
    echo " "
    exit 2


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#  EXIT STATUS SUMMARY                                                  #
#                                                                       #
#  99 - Problem with Environmental Variables                            #
#  98 - Bad imput parameters                                            #
#  97 - Model already running                                           #
#  96 - Data not kept on ftp server                                     #
#                                                                       #
#  59 - Missing executable                                              #
#  58 - Problem with conversion processes                               #
#                                                                       #
#  29 - Missing grib files on ftp server                                #
#  28 - GRIB files not correctly downloaded                             #
#  27 - Missing surface files                                           #
#                                                                       #
#   1 - Interupt                                                        #
#   0 - normal exit                                                     #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
