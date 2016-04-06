#! /bin/csh
#  Get GRIB files from SAC and then rcp to AWIPS
#  Assumes the model is run on a machine outside the AWIPS network.
#
#  Author: John Eise SOO MKX  October 2000
#  Update 11/30/00 Changed IP address to SAC5 where WSETA now runs
#         12/22/00 Added support to send UW-NMS inner grid to AWIPS
#
# How to run: /home/eta/get_grib.csh TT where TT=00,06,12,18 (model 00 hour)
#
# Sample crontab entries, assumes LINUX box set to CDT. This gets the data twice and
# sends it to awips via rcp command.
#
# 30 00,01 * * * /bin/csh /home/eta/get_grib.csh 00 >> /tmp/get_grib00.log 2>&1
# 30 06,07 * * * /bin/csh /home/eta/get_grib.csh 06 >> /tmp/get_grib06.log 2>&1
# 30 12,13 * * * /bin/csh /home/eta/get_grib.csh 12 >> /tmp/get_grib12.log 2>&1
# 30 18,19 * * * /bin/csh /home/eta/get_grib.csh 18 >> /tmp/get_grib18.log 2>&1
#
# Make sure you have the file .rhosts in your home directory. Sample file (make sure
# each line below is left justified in the .rhosts file):
#
#  as1-mkx fxa
#  as2-mkx fxa
#  as1-mkx www
#  as2-mkx www
#  ds1-mkx fxa
#  ds2-mkx fxa
#  ws1-mkx fxa
#  ws2-mkx fxa
#  ws3-mkx fxa
#  ws4-mkx fxa
#  ws5-mkx fxa
#  appl-mkx eta
#
# NOTE: This exact same .rhosts file must also be on /awips/fxa on DS1 and assumes
#       you are coming from the ETA account on your local machine.
#
#####################################################################################
# Set your variables here
#   IP       = IP address of the machine you will download from
#   username = user name of machine you will download from
#   password = password of machine you will download from
#   local_grib_dir  = local directory that you will store the GRIB files you download
#   remote_grib_dir = remove directory from which you will download the GRIB files
#   awips_dir       = directory on AWIPS where the GRIB files will be rcp'd
#####################################################################################
set IP = '204.194.229.45'
set username = 'eta'
set password = 'gobble!'
set local_grib_dir = '/usr1/WSETA'
set remote_grib_dir = '/usr1/WSETA'
set metdat_grib_dir = '/usr1/metdat/raw/grib'
set awips_dir = 'fxa@ds1:/data/fxa/Grid/SBN/Raw'
#####################################################
# No need to change anything below this point       #
#####################################################
#
# Leave these variables alone.
#
set yy = `date -u +%y`
set mm = `date -u +%m`
set dd = `date -u +%d`
set hh = $argv[1]
set file = ${yy}${mm}${dd}${hh}'.Grb*'
set file_nms = 'us008_gf110_'${yy}${mm}${dd}${hh}'_*'
#
# Set environmental variables
#
source /home/eta/Common.cshrc
#
# Prepare for ftping from SAC
#
onintr cleanup
cd /usr1/WSETA
echo "Start FTPing from SAC"
ftp -n $IP << EOF
user $username $password
prompt off
hash on
bin
cd $remote_grib_dir
lcd $local_grib_dir
mget $file
cd $metdat_grib_dir
mget $file_nms
bye
EOF
endif
echo "Finished FTPing files"
echo "Files downloaded from " $IP
ls -al $file
ls -al $file_nms
#
# chmod files
#
echo "Now chmoding"
chmod a+rwx *
pwd
echo "Files chmod'd in directory " $local_grib_dir
cd $local_grib_dir
ls -al $file
ls -al #file_nms
#
# rcp files to AWIPS
#
echo "Now rcping files to " $awips_dir
rcp $file $awips_dir
rcp $file_nms $awips_dir
echo "Finished rcping files to " $awips_dir
#
# unset variables
#
cleanup:
onintr -
unset yy
unset mm
unset dd
unset hh
unset file
unset IP
unset username
unset password
unset local_grib_dir
unset remote_grib_dir
unset awips_dir
#
exit
