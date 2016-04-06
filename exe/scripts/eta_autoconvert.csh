#!/bin/csh  -f
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#  ETA_AUTOCONVERT       Version 2.0                                    #  
#                                                                       #
#  The eta_autoconvert.csh program is used for converting the raw       #
#  workstation ETA data "on the fly", i.e, while the model is running.  #
#  This script is called from the eta_run.csh program and will auto-    #
#  matically terminate upon completion of the eta model run. The script #
#  looks for an eta_lock file to determine if the model is running, and #
#  if found, will call eta_convert.csh to process the data. To initiate #
#  the autoconversion processes, simply include an "A" as an argument   #
#  the eta_run.csh program:                                             #
#                                                                       #
#      % eta_run.csh A                                                  #
#                                                                       #
#  Note that setting the AUTO_CONVERT flag to "yes" in the              #
#  eta_autorun.csh script will accomplish the same thing.               #
#                                                                       #
#  LOG:                                                                 #
#       R.Rozumalski - 03/00    Version 1.0 initial release             #
#       R.Rozumalski - 04/01    Version 2.0 initial release             #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#           ETA_AUTOCONVERT SCRIPT CONFIGURATION SECTION                #
#           THERE IS NOT A LOT TO CONFIGURE                             #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
#      WAIT                                                             #
# 1.   WAIT is the amount of time (seconds) to wait before attempting   #
#      to convert output from the workstation eta model.  You should    #
#      set this value greater than the frequency of model output.       #
#                                                                       #
#########################################################################
set WAIT = 1200

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      END OF ETA_AUTOCONVERT SCRIPT CONFIGURATION SECTION              #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#
#  Begin the eta_autoconvert.csh script.
#
if ( ! -e $ETA_TMP/eta_lock ) then
   echo " "
   echo "The lockfile is missing ($ETA_TMP/eta_lock). Did the model die?"
   echo " "
   exit 99
endif

if ( -e $ETA_TMP/auto_convert.log ) rm -f $ETA_TMP/auto_convert.log

sleep $WAIT

set ETA_RUNNING = 1
while ( $ETA_RUNNING )

    if (  ! -e $ETA_TMP/eta_lock ) set ETA_RUNNING = 0

    echo "Executing eta_convert.csh - `date`" >>& $ETA_TMP/auto_convert.log
    $ETA_SCRIPTS/eta_convert.csh              >>& $ETA_TMP/auto_convert.log
    echo "sleeping $WAIT seconds Zzzzzzzzz"   >>& $ETA_TMP/auto_convert.log
    echo " "                                  >>& $ETA_TMP/auto_convert.log
    if ( $ETA_RUNNING ) sleep $WAIT

end

echo " "                                    >>& $ETA_TMP/auto_convert.log
echo "Exiting eta_autoconvert.csh - `date`" >>& $ETA_TMP/auto_convert.log
echo " "                                    >>& $ETA_TMP/auto_convert.log

if ( -e $ETA_TMP/convert_lock ) rm -f $ETA_TMP/convert_lock
exit 0

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                   END OF ETA_AUTOCONVERT SCRIPT                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
