#! /bin/csh -f
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#  ETA_CONVERT           Version 2.0                                    #
#                                                                       #
#  This script will convert the raw workstation ETA model output into   #
#  a format suitable for viewing in either GEMPAK or AWIPS.  The script #
#  can either take arguments, POST_START, POST_STOP, and POST_INT or    #
#  can run without any arguments from the settings defined below.       #
#  The script will also run with just the single argument - ALL. Please #
#  see the discussion below for details.                                #
#                                                                       #
#  Please send any comments, suggestions, or bug fixes to:              #
#  Robert.Rozumalski@noaa.gov                                           #
#                                                                       #
#  LOG:                                                                 #
#       R.Rozumalski - 03/00    Version 1.0 - Initial Release           #
#       R.Rozumalski - 07/00    Version 1.1                             #
#       R.Rozumalski - 04/01    Version 2.0                             #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
# 1. POSTPROCESS  { YES | NO }                                          #
#                                                                       #
#    set POSTPROCESS = YES if you want to write the raw workstation Eta #
#    model output into GRIB files for viewing in display software such  #
#    as NAWIPS/GEMPAK, AWIPS, or GRADS.  You should always have         #
#    POSTPROCESS = YES unless you just want to ship the eta quilt files #
#    to another machine or archive them for later use.                  #
#                                                                       #
#########################################################################
set POSTPROCESS = YES

#  Configuration required if  POSTPROCESS = YES
#
#  Set SAVE_GRIB = YES if you want to save the processed GRIB files for
#  future use. GRIB_DIR is a directory on the LOCAL machine to hold the
#  GRIB files.  You MUST set POSTPROCESS = YES if you want to use the
#  Eta model data for display in another package such as AWIPS or NAWIPS.
#
#  Note that even if POSTPROCESS = YES

set SAVE_GRIB = YES
set GRIB_DIR  = $ETA_DATA/model_gribs

#  Configuration required if  POSTPROCESS = NO
#
#  QUILT_DIR is the location of the directory on the local machine to
#  put the model output files (quilt files) after the model runs. If
#  QUILT_DIR = " " or QUILT_DIR = ./ then the files will be deleted
#  after all the post-processing is completed.  Unless you want to
#  keep the raw model output files for some reason it is better to
#  save space and delete them. If POSTPROCESS = NO you probably want
#  to keep the quilt files.
#
set SAVE_QUILT = NO
set QUILT_DIR  = $ETA_DATA/model_quilts



#########################################################################
# 2. AWIPS  ( FTP | RCP | NO)                                           #
#                                                                       #
#    Set AWIPS to either FTP or RCP if you wish to move the processed   #
#    GRIB files into AWIPS for viewing in D2D. If you plan on using     #
#    this option, it is imperative that you read wseta_2awips.README    #
#    located in the $ETA_DOCS directory. Note that the data are not     #
#    converted to NetCDF format here, but rather, remote copy (RCP) or  #
#    FTP commands are used to push the WS Eta GRIB files to your AWIPS  #
#    system for further processing.                                     #
#                                                                       #
#    NOTE:  For those of you running the eta_convert outside the AWIPS  #
#           firewall, there is a another way of getting the WS Eta GRIB #
#           data into AWIPS. Please read wseta_2awips.README for more   #
#           information.  You will need to set SAVE_GRIB = YES and run  #
#           make sure get_grib.csh has access to the files.             #
#                                                                       #
#    ACKNOWLEDGEMENT: The instructions to move the WS Eta data into     #
#                     AWIPS were provided by John Eise (SOO/MKX) and    #
#                     Dan Baumgardt (SOO/ARX). They are wonderful and   #
#                     wonderfully when implemented correctly; however,  #
#                     on the remote chance that you have a problem with #
#                     them, please contact me first                     #
#                                                                       #
#    ALSO: In order to move your WS Eta GRIB files into AWIPS for       #
#          processing into NetCDF you MUST have a .rhosts file          #
#          configured on your local machine in the user "eta" account.  #
#          Please see wseta_2awips.README for more information.         #
#                                                                       #
#########################################################################
set AWIPS = NO

#    Further configuration required if AWIPS = (RCP or FTP)
#
#    RCP
#    If you are running eta_convert.csh bewind the AWIPS firewall, you
#    may use rcp to get the GRIB files to the /data/fxa/Grid/SBN/Raw
#    directory on your ds1 machine. Make sure that you have read the
#    wseta_2awips.README file and that you have write permission on 
#    ds1  and that your .rhosts files are correctly configured.
#    Example:  set REM_AWIPS_DIR = 'fxa@ds1:/data/fxa/Grid/SBN/Raw'

#set REM_AWIPS_DIR = 'fxa@ds1:/data/fxa/Grid/SBN/Raw'

#    FTP
#    If you are running the WS Eta outside the AWIPS firewall you can
#    use FTP to transfer you data to another machine such as AWIPS LDAD
#    or ingest for processing and display. Make sure that the machine
#    information is located in your .netrc file!
#
set  AWIPS_MACHINE = awips.ip.address
set  REM_AWIPS_DIR = /data/Incoming



#########################################################################
# 3. NAWIPS  ( YES | NO )                                               #
#                                                                       #
#    Set NAWIPS = YES if you want to processes the GRIB files for       #
#    viewing in NAWIPS/GEMPAK.  If NAWIPS = YES then the WS Eta GRIB    #
#    files will be converted into GEMPAK format by this script resulting#
#    in a file named YYMMDDHH_wseta.gem containing ALL forecast output  #
#    times.                                                             #
#                                                                       #
#    Note:  NAGRIB binaries for LINUX and HPUX are provided as part     #
#           of the WS Eta package, so there is no need to install the   #
#           NAWIPS distribution for creating GEMPAK files. However,     #
#           if you plan on running eta_convert.csh on a different       #
#           platform you will need to either change the $ETA_EXE/nagrib #
#           refrence to $GEMEXE on your system or copy nagrib, gpend,   #
#           and gplt to the appropriate $ETA_EXE directory.             #
#                                                                       #
#########################################################################
set NAWIPS = YES

#    Further configuration required if NAWIPS = YES
#

# Set LOCAL = NO if you do not have NAWIPS available to convert the
# GRIB files into GEMPAK format. If LOCAL = NO, then the NAWIPS executables
# located in $ETA_EXE will be used.
#
set LOCAL = NO

# Set GEM_FTP = YES if you want to ftp your GEMPAK files to another
# machine after creation. REM_GEM_DIR is the directory location on the 
# remote machine. GEM_MACHINE is the IP address of the remote machine.
# NOTE that if must have the remote machine's login and password info
# in the .netrc file at the top level of WS Eta user's account.
#
set GEM_FTP     = NO
set GEM_MACHINE = soosac.comet.ucar.edu
set REM_GEM_DIR = for_bob

# LOC_GEM_DIR is a directory on your local machine to copy the data 
# following conversion into GEMPAK format.  If no directory is specified
# then the GEMPAK file will be left in the WS Eta run_time directory
# and deleted upon completion on a new model run.
#
set LOC_GEM_DIR = "./"


#########################################################################
# 4. POST_START, POST_STOP, POST_INT                                    #
#                                                                       #
#    POST_START, POST_STOP, and POST_INT are the beginning time, ending #
#    time, and interval (in hours) of the raw ETA model output to       #
#    convert.  These values may be over written by providing arguments  #
#    to this script:                                                    #
#                                                                       #
#       >  eta_convert.csh  POST_START  POST_STOP  POST_INT             #
#    or                                                                 #
#       >  eta_convert.csh  ALL                                         #
#                                                                       #
#                                                                       #
#    The ALL argument will convert all available WS ETA Model output in #
#    the runtime directory. Note that setting POST_START = ALL below    #
#    will also have the same results.  In that case POST_STOP and       #
#    POST_INT are ignored.                                              #
#                                                                       #
#########################################################################
set POST_START = ALL
set POST_STOP  = 36
set POST_INT   = 01

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#    END OF PRIMARY CONFIGURATION.  THERE IS NO NEED TO CHANGE          #
#    ANYTHING BELOW THIS LINE.                                          #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#########################################################################
# 5. GRID_NAV                                                           #
#                                                                       #
#    GRID_NAV defines the navigation of the post-processed ETA Model    #
#    data in GRIB format.  The choices are either lambert conformal     #
#    (LMBC) or latitude-longitude (LATLON).                             #
#                                                                       #
#########################################################################
set GRID_NAV = LMBC

#########################################################################
# 6. POST_CONTROL                                                       #
#                                                                       #
#    POST_CONTROL specifies the file to use for controlling the output  #
#    to the grib files.  An in-depth discussion of the contents of this #
#    file is beyond the ability of my fingers to type.  A more verbose  #
#    description of this file and it contents can be found in the NCEP  #
#    users manual located in the $ETA_DOC directory. The section on     #
#    the post processor details everything you will ever  need to know  #
#    about adding/deleting fields, scaling, and smoothing of the data.  #
#                                                                       #
#    For the most part, it is unnecessary to modify the POST_CONTROL    #
#    file as the current configuration will provide almost everything   #
#    you need from model output and more.                               #
#                                                                       #
#########################################################################
set POST_CONTROL = cntrl.parm



unalias rm
umask 000


#  Define the ETA_HOME directory again, just to make sure
#
env | grep WS_ETA >& /dev/null
if ( $status != 0 ) then
    echo WS_ETA is not defined.
    exit 1
endif

if ( -e $WS_ETA/ETA.cshrc  ) then
   source $WS_ETA/ETA.cshrc 
else
   echo "ERROR: $WS_ETA/ETA.cshrc is not found."
   echo "       Go find it and try again."
   echo " "
   exit 1
endif

find $ETA_TMP -name convert_lock -atime +0 -exec rm -f {} \; >& /dev/null
if ( -e $ETA_TMP/convert_lock ) then
   echo " "
   echo "The convert script appears to be already running. If this is in error,"
   echo "then remove the convert_lock file from the ETA_TMP directory and run "
   echo "this script again. If you are using the AUTO_CONVERT option, increase "
   echo "the amount of time between calls in eta_autoconvert.csh program. "
   echo "Otherwise, just sit on your hands for a few minutes and try again. - EXIT"
   echo " "
   exit 1
endif

cd $ETA_RUN
ls restrt??.quilt >& /dev/null
if ( $status != 0 ) then
   echo "There were no restart files found - EXIT"
   echo " "
   exit 1
endif

if ( -e $ETA_TMP/eta_lock ) then
   set AUTO_CONVERT = yes
else
   set AUTO_CONVERT = no
endif

#  MAKE SURE THAT VARIABLES ARE ALL LOWER CASE
#
set POSTPROCESS = `echo $POSTPROCESS | tr '[A-Z]' '[a-z]'`
set AWIPS       = `echo $AWIPS       | tr '[A-Z]' '[a-z]'`
set NAWIPS      = `echo $NAWIPS      | tr '[A-Z]' '[a-z]'`
set SAVE_GRIB   = `echo $SAVE_GRIB   | tr '[A-Z]' '[a-z]'`
set SAVE_QUILT  = `echo $SAVE_QUILT  | tr '[A-Z]' '[a-z]'`
set GEM_FTP     = `echo $GEM_FTP     | tr '[A-Z]' '[a-z]'`
set LOCAL       = `echo $LOCAL       | tr '[A-Z]' '[a-z]'`


set GRID_NAV    = `echo $GRID_NAV    | tr '[a-z]' '[A-Z]'`

touch  $ETA_TMP/convert_lock

onintr clean_up

if ( $LOCAL != y && $LOCAL !=  yes ) setenv GEMEXE $ETA_EXE


echo " "
echo "#############################################################"
echo "############ Let the Conversion Script Begin ################"
echo "### Starting ETA_CONVERT -  `date` ####"



#  CHECK USER INPUT. GENERATE AN ERROR IF THE INPUT IS INCORRECT.
#

if ( $#argv > 0 ) then
    if ( $1 == ALL || $1 == all ) then
        set POST_START = ALL
        
    else if ( $#argv == 3 ) then
        set POST_START = $1
        set POST_STOP  = $2
        set POST_INT   = $3

    else
        echo " "
        echo "  The ETA_CONVERT script:"
        echo " "
        echo "      >  eta_convert.csh  POST_START  POST_STOP  POST_INT"
        echo "  or"
        echo "      >  ALL"
        echo " "
        echo "  Where POST_START, POST_STOP, and POST_INT are the beginning"
        echo "  time, ending time, and interval (in hours) of the raw WS ETA"
        echo "  model data to convert."
        echo " "
        if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
        exit 2

    endif
endif

ls init.???????? >& /dev/null
if ( $status != 0 ) then
   echo " "
   echo "OOPS: The initialization file, init.YYMMDDHH, is missing, where YY, MM,"
   echo "DD, and HH are the Year, Month, Day, and Hour of the 00 hour forecast."
   echo "If you have inadvertently deleted this file, your sole is not lost. "
   echo "Simply use the touch command with the appropriate filename and rerun"
   echo "this script."
   echo "For Example: "
   echo "            %  touch init.01020412"
   echo " "
   if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
   exit 2
endif

set DATTIMS   = ( `ls -t init.???????? | cut -d"." -f2` )
set DATTIM    = $DATTIMS[1]



#  IDENTIFY THE MODEL QUILT FILES TO BE CONVERTED TO GRIB
#
if ( $POST_START == ALL ) then

    set RAW_ETA_FILES = ( `ls restrt??.quilt` ) 

else
    if ( -e .tmpfile ) rm -f .tmpfile

    set OF = $POST_START
    while ( $OF <= $POST_STOP )
        set cnts = ( `echo $OF | wc` )
        if ( $cnts[3] < 3 ) set OF = 0$OF

        if ( -e restrt${OF}.quilt ) echo restrt${OF}.quilt >> .tmpfile
        @ OF = $OF + $POST_INT
    end

    set RAW_ETA_FILES = ( `cat .tmpfile` )

    rm -f .tmpfile
endif

#  IF THERE ARE NO FILES IDENTIFIED, THEN EXIT
#
set NUM_FILES  = $#RAW_ETA_FILES
if ( $NUM_FILES == 0 ) then
    echo "OOPS - The is a problem with your ETA model data."
    echo "       There isn't any - EXIT"
    if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
    exit 3
endif


#  IF NO POST PROCESSING WAS REQUESTED THEN DO SOMETHING WITH THE
#  QUILT FILES.
#
if ( $POSTPROCESS != yes ) then
   echo " "
   echo "  WARNING:  POSTPROCESS is set to $POSTPROCESS, so the model"
   echo "            output files are only going to be moved and no"
   echo "            further processing will be done at this tile."
   echo " "
   echo "  Moving $RAW_ETA_FILES to ${QUILT_DIR}/${DATTIM}
   if ( ! -e ${QUILT_DIR} ) mkdir ${QUILT_DIR}
   if ( ! -e ${QUILT_DIR}/${DATTIM} ) mkdir ${QUILT_DIR}/${DATTIM}
   mv $RAW_ETA_FILES ETAIN fcstdata ${QUILT_DIR}/${DATTIM}
   echo " "
   echo "EXIT at `date`."
   echo " "
endif



#  COMPUTE THE FREQUENCY OF THE MODEL DATA FROM THE LIST
#  OF FILES REQUESTED.  NOTE THAT THIS IS REDUNDANT EXCEPT
#  FOR THE "ALL" ARGUMENT, BUT IT SAVES CODE.
#
set INIT  =  ( `echo $RAW_ETA_FILES[1] | cut -c7-8` )
set FINL  =  ( `echo $RAW_ETA_FILES[$NUM_FILES] | cut -c7-8` )
@   BCLEN =  ${FINL} - ${INIT}
if ( ${NUM_FILES} > 1 ) then
   @   TBOCO =  ${BCLEN} / ( ${NUM_FILES} - 1 )
else
   set TBOCO = 0
endif

echo $INIT $NUM_FILES $TBOCO > .post_data



#  RUN TEH QUILT PROGRAM TO COMPUTE THE PMSL FIELDS AND 
#  REORGANIZE THE DATA.
#
if ( ! -e $ETA_EXE/quilt.exe ) then
    echo " "
    echo "ERROR:  $ETA_EXE/quilt.exe was not found."
    echo "        Go find it and try again. "
    echo " "
    echo "EXIT at `date`."
    echo " "
    if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
    exit 2
endif


#  MAKE SOME NECESSARY LINKS
#
if ( $ETA_OS == hpux ) then
    set head = ftn
else
    set head = fort.
endif

ln -s -f fcstdata                   ${head}11
ln -s -f cnst.file                  ${head}12
ln -s -f $ETA_DATA/const/cntrl.parm ${head}14



#  BEGIN THE ACTUAL POST PROCESSING OF THE DATA. FIRST RUN quilt.exe
#  FOLLOWED BY etapost.exe, AND THEN copygb.exe.
#
echo " "
echo "  Running the ETA post processor on the following raw "
echo "  Workstation ETA files:"
echo " "
foreach ETA_FILE ( $RAW_ETA_FILES )
    echo "    $ETA_FILE"
    set TM = ( `echo $ETA_FILE | cut -c7-8` )
    if ( -e EGRD3D.GrbF${TM}    ) rm -f EGRD3D.GrbF${TM}
    if ( -e ${DATTIM}.GrbF${TM} ) rm -f ${DATTIM}.GrbF${TM}
end

echo " "
echo "  Running quilt.exe to compute sea level pressure."
echo "  Output can be found in $ETA_TMP/eta_quilt.log"
echo " "
$ETA_EXE/quilt.exe < .post_data >& $ETA_TMP/eta_quilt.log

grep STOP_QUILT $ETA_TMP/eta_quilt.log >& /dev/null
if ( $status == 0 ) then
   echo "  Done\! - quilt.exe was successful."
   echo " "
else
   echo "  ERROR:  QUILT failed\!"
   echo "  Here are the last few lines from $ETA_TMP/eta_quilt.log"
   echo "  "
   tail -10 $ETA_TMP/eta_quilt.log
   echo "  "
   if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
   exit 2
endif

set SUCCESS = 0
foreach FILE ($RAW_ETA_FILES)
    set TMPFILE = `echo $FILE | cut -d"." -f1`
    if ( ! -e $TMPFILE ) then
       echo "  ERROR:  QUILT failed\! $TMPFILE was not found."
       set SUCCESS = 1
    endif
end

if ( $SUCCESS == 1 ) then
   echo "    Here are the last few lines from $ETA_TMP/eta_quilt.log"
   echo "  "
   echo "  tail -10 $ETA_TMP/eta_quilt.log
   echo "  "
   if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
   exit 2
endif

#  RUN THE ETA POST PROCESSOR.  THIS STEP WILL CREATE AN
#  INTERMEDIATE GRIB FILE THAT WILL BE REMAPPED TO A MORE
#  USEFUL GRID NAVIGATION.
#
if ( ! -e $ETA_EXE/etapost.exe ) then
    echo " "
    echo "ERROR:  $ETA_EXE/etapost.exe was not found."
    echo "        Go find it and try again. "
    echo " "
    echo "EXIT at `date`."
    echo " "
    if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
    exit 2
endif

echo " "
echo "  Running etapost.exe"
echo "  Output can be found in $ETA_TMP/eta_post.log"
echo " "
$ETA_EXE/etapost.exe < .post_data >& $ETA_TMP/eta_post.log

grep STOP_POST $ETA_TMP/eta_post.log >& /dev/null
if ( $status == 0 ) then
   echo "  Done\! - etapost.exe was successful."
   echo " "
else
   echo "  ERROR:  POST failed\!"
   echo "  Here are the last few lines from $ETA_TMP/eta_post.log"
   echo "  "
   tail -10 $ETA_TMP/eta_post.log
   echo "  "
   if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
   exit 2
endif



ls EGRD3D.GrbF?? >& /dev/null
if ( $status == 0 ) then
   rm -f restrt?? >& /dev/null
else
   echo "  ERROR:  ETA_POST.EXE failed\! - EXIT"
   echo "          Here are the last few words from $ETA_TMP/eta_post.log"
   echo " "
   tail -10 $ETA_TMP/eta_post.log
   echo " "
   if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
   exit
endif


if ( $GRID_NAV == LMBC ) then
    set navinfo = ( `cat outjob_input_lmbc` )
else if ( $GRID_NAV == LATLON ) then
    set navinfo = ( `cat outjob_input_lat` )
else
    echo "ERROR:  Output grid navigation (GRID_NAV) not set - EXIT"
    if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
    exit 2
endif

set ETA_FILES = ( `ls EGRD3D.GrbF??` )



#  COPYGB.EXE  NEEDS TO BE LOCATED IN THE CURRENT WORKING DIRECTORY.
#  SO, COPY THE FILE LOCALLY, RUN THE ROUTINE, AND DELETE AFTERWARDS.
#
if ( -e $ETA_EXE/copygb.exe ) then
    cp $ETA_EXE/copygb.exe .
else
    echo " "
    echo "  ERROR:  $ETA_EXE/copygb.exe was not found."
    echo "          Go find it and try again. "
    echo "EXIT at `date`."
    echo " "
    if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
    exit 2
endif



#  RUN COPYGB.EXE TO CONVERT THE INTERMEDIATE GRIB DATA TO TEH
#  REQUESTED GRID NAVIGATION.
#
echo "  Running the COPYGB program to change grid navigation to $GRID_NAV"
echo "  Output can be found in $ETA_TMP/copygb.log"
echo " "

set OUT_GRIBS = `echo $ETA_FILES`

set N = 1

foreach ETA_FILE ( $ETA_FILES )

    set FCST = ( `echo $ETA_FILE | cut -d"." -f2` )
    set OUT_GRIB = ${DATTIM}.${FCST}
    set OUT_GRIBS[${N}] = $OUT_GRIB
    
    echo "    Creating final GRIB file: $OUT_GRIB"
    echo $navinfo  > $ETA_TMP/copygb.log
    ./copygb.exe -g"$navinfo" -s"2,-3" -x $ETA_FILE $OUT_GRIB >> $ETA_TMP/copygb.log

    if ( -e $OUT_GRIB ) rm -f $ETA_FILE

    @ N = $N + 1

end



##########################################################################
#  IF THERE IS ANY POST PROCESSING TO BE DONE AFTER THE GRIB FILES       #
#  ARE CREATED, IT SHOULD GO BELOW.                                      #
##########################################################################
#
if ( $AWIPS == rcp ) then

#  SO, YOU WANT TO VIEW THESE DATA IN AWIPS? - GOOD FOR YOU
#  USING RCP TO MOVE DATA.
#
   echo "Remote Copying GRIB files to AWIPS Machine"
   echo " "
   /usr/bin/rcp  $OUT_GRIBS  $REM_AWIPS_DIR

else if ( $AWIPS == ftp ) then

#  USING FTP TO MOVE THE DATA INTO AWIPS
#

   grep $AWIPS_MACHINE ~/.netrc >& /dev/null
   if ( $status != 0 ) then
      echo "  OOPS:  You will need to add $AWIPS_MACHINE to your .netrc file."
      echo "         No data will be Ftp'd to $AWIPS_MACHINE."
      echo " "
   else

#  LETS FTP THE GEMPAK DATA SOMEWHERE
#
#  CHECK TO SEE IF THE TARGET MACHINE IS UP. IF NOT, THEN DON'T TRY
#  TO FTP THE DATA AND SEND WARNING.

      if ( $ETA_OS == hpux ) then
         /usr/sbin/ping $AWIPS_MACHINE 256 2 >& /dev/null
      else if ( $ETA_OS == linux ) then
         /bin/ping -c 2 -s 256 $AWIPS_MACHINE >& /dev/null
      endif

      if ( $status != 0 ) then
         echo " "
         echo "  WARNING: $AWIPS_MACHINE is not responding to a ping. "
         echo "  No model data will be uploaded to that computer."
         echo " "
      else
         echo " "
         echo "  FTPing GRIB files to the $REM_AWIPS_DIR directory on $AWIPS_MACHINE"
         echo "  Information can be found in $ETA_TMP/awipsftp.log"
         echo " "

         echo "binary"              > $ETA_TMP/ftpfile_awips.$$
         echo "prompt off"         >> $ETA_TMP/ftpfile_awips.$$
         echo "umask 111"          >> $ETA_TMP/ftpfile_awips.$$
         echo "cd $REM_AWIPS_DIR"  >> $ETA_TMP/ftpfile_awips.$$
         echo "mput $OUT_GRIBS"    >> $ETA_TMP/ftpfile_awips.$$
         echo "bye"                >> $ETA_TMP/ftpfile_awips.$$
         ftp -v $AWIPS_MACHINE < $ETA_TMP/ftpfile_awips.$$ > $ETA_TMP/awipsftp.log
         rm -f $ETA_TMP/ftpfile_awips.$$
      endif

   endif

endif


if ( $NAWIPS == yes ) then

#  IF FORMAT = GEMPAK, THEN CREATE A FILE TO DRIVE THE GEMPAK
#  CONVERSION.  ALSO, CHECK TO SEE IF NAGRIB IS AVAILABLE.
#
#    GET NAME FOR GEMPAK GRIDDED DATA FILE. DELETE 
#    EXISTING GRIDS TO SAVE SPACE.
#
   set GEM_GRID = ${DATTIM}_wseta.gem

   if ( ! -e $GEM_GRID ) then
      ls *_wseta.gem >& /dev/null
      if ( $status == 0 ) rm -f *_wseta.gem
   endif

   if ( ! -e $ETA_EXE/nagrib ) then
      echo " "
      echo "ERROR:  $ETA_EXE/nagrib was not found."
      echo "        Go find it and try again. "
      echo " "
      echo "EXIT at `date`."
      echo " "
      if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
      exit 2
   endif

   if ( -e gemfile ) rm -f gemfile

   echo \$RESPOND = YES                >  gemfile
   echo "INDXFL   = "                  >> gemfile
   echo "GDOUTF   = $GEM_GRID"         >> gemfile
   echo "GAREA    = "                  >> gemfile
   echo "GRDAREA  = "                  >> gemfile
   echo "KXKY     = "                  >> gemfile
   echo "MAXGRD   = 18000"             >> gemfile
   echo "PROJ     ="                   >> gemfile
   echo "CPYFIL   = GDS"               >> gemfile
   echo "OUTPUT   = t"                 >> gemfile
   echo "GBTBLS   = w.tbl;n.tbl;v.tbl;c.tbl" >> gemfile
   echo "GBDIAG   = "                  >> gemfile

   cp $ETA_DATA/const/w.tbl  $ETA_DATA/const/n.tbl .
   cp $ETA_DATA/const/v.tbl  $ETA_DATA/const/c.tbl .

   foreach OUT_GRIB ( $OUT_GRIBS )
      echo "GBFILE   = $OUT_GRIB"      >> gemfile
      echo "run        "               >> gemfile
      echo "           "               >> gemfile
   end

   echo "exit            "             >> gemfile
   echo "${GEMEXE}/gpend "             >> gemfile

   echo " "
   echo "  Writing the GRIB data to GEMPAK file: $GEM_GRID"
   echo "  Output can be found in $ETA_TMP/gempak.log"
   echo " "

   if ( -e $ETA_TMP/gempak.log ) rm -f $ETA_TMP/gempak.log

   cat gemfile > $ETA_TMP/gempak.log
   ${GEMEXE}/nagrib < gemfile >> $ETA_TMP/gempak.log
   ${GEMEXE}/gpend >& /dev/null
   rm -f gemfile

   if ( -e $GEM_GRID ) then

      if ( $GEM_FTP == yes ) then

#     CHECK TO SEE IF THE REMOTE SERVER IS LISTED IN THE USERS .netrc FILE
#
         grep $GEM_MACHINE ~/.netrc >& /dev/null
         if ( $status != 0 ) then
            echo "  OOPS:  You will need to add $GEM_MACHINE to your .netrc file."
            echo "         No data will be Ftp'd to $GEM_MACHINE."
            echo " "
         else

#           LETS FTP THE GEMPAK DATA SOMEWHERE
#
#           CHECK TO SEE IF THE TARGET MACHINE IS UP. IF NOT, THEN DON'T TRY
#           TO FTP THE DATA AND SEND WARNING.

            if ( $ETA_OS == hpux ) then
               /usr/sbin/ping $GEM_MACHINE 256 2 >& /dev/null
            else if ( $ETA_OS == linux ) then
               /bin/ping -c 2 -s 256 $GEM_MACHINE >& /dev/null
            endif

            if ( $status != 0 ) then
               echo "  WARNING: $GEM_MACHINE is not responding to a ping. "
               echo "  No model data will be uploaded to that computer."
               echo " "
            else
               echo "  FTPing $GEM_GRID to the $REM_GEM_DIR directory on $GEM_MACHINE"
               echo "  Information can be found in $ETA_TMP/ftp.log"
               echo " "

               echo "binary"          >  $ETA_TMP/ftpfile.$$
               echo "cd $REM_GEM_DIR" >> $ETA_TMP/ftpfile.$$
               echo "put $GEM_GRID"   >> $ETA_TMP/ftpfile.$$
               echo "bye"             >> $ETA_TMP/ftpfile.$$
               ftp -v $GEM_MACHINE < $ETA_TMP/ftpfile.$$ > $ETA_TMP/ftp.log
               rm -f $ETA_TMP/ftpfile.$$
            endif

         endif

      endif

#     MOVE THE DATA TO A LOCAL DIRECTORY IF REQUESTED
#
      if ( $LOC_GEM_DIR != ./ && $LOC_GEM_DIR != " " && $LOC_GEM_DIR != "" ) then
         echo "  COPYING: $GEM_GRID to $LOC_GEM_DIR"
         echo " "
         cp $GEM_GRID $LOC_GEM_DIR
      endif

   else
      echo " "
      echo "ERROR:  $GEM_GRID was not found\! This is not good."
      echo " "
   endif
   
endif

#  END IF POST PROCESS SECTION

#  YOU NEED TO DETERMINE WHAT TO DO WITH ALL THE DATA YOU'VE JUST
#  CREATED.  YOU CAN FTP IT, MOVE IT, OR BOTH.  PLEASE READ THE
#  CONFIGURATION SECTION AT THE TOP OF THIS SCRIPT FOR MORE DETAILS.
#

####################### MOVE THE DATA #############################
#  IF THE MODEL DATA FILE IS TO BE MOVED FROM THE RUNTIME DIRECTORY
#  IT IS DONE HERE.
#
#  IF DESIRED, SAVE THE RAW GRIB AND RESTART FILES. RESTART FILES
#  NAMES ARE HELD IN $RAW_ETA_FILES
#

if ( $SAVE_QUILT == yes ) then
   if ( $QUILT_DIR != ./ && $QUILT_DIR != " " && $QUILT_DIR != "" ) then
      if ( ! -d $QUILT_DIR ) mkdir $QUILT_DIR
      if ( ! -d ${QUILT_DIR}/${DATTIM} ) mkdir ${QUILT_DIR}/${DATTIM}
#      mv $RAW_ETA_FILES ETAIN fcstdata $QUILT_DIR/${DATTIM}
      mv $RAW_ETA_FILES $QUILT_DIR/${DATTIM}
      cp ETAIN fcstdata $QUILT_DIR/${DATTIM}
   endif
else
   rm -f $RAW_ETA_FILES >& /dev/null
endif

if ( $SAVE_GRIB == yes ) then
   if ( ! -d $GRIB_DIR ) mkdir $GRIB_DIR
   if ( ! -d ${GRIB_DIR}/${DATTIM} ) mkdir ${GRIB_DIR}/${DATTIM} 
   mv $OUT_GRIBS  ${GRIB_DIR}/${DATTIM}
else
   rm -f $OUT_GRIBS >& /dev/null
endif

#  CLEAN UP ANY UNNECESSARY FILES
#

clean_up:

if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock

rm -f copygb.exe restrt?? *.tbl   rdtnd*   >& /dev/null
rm -f ${head}?? .gemfile .post_data  *.nts >& /dev/null
rm -f outjob_input_lmbc outjob_input_lat   >& /dev/null

echo " "
echo "############# Conversion Script Completed ###################"
echo "### ETA_CONVERT Completed - `date` ####"
echo "#############################################################"
echo " "

sync
if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
exit 0
