#!/bin/csh 
#
#    
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#  ETA_RUN              Version 2.0                                     #
#                                                                       #
#  The ETA_RUN script configures and runs the ETA preprocessor and      #
#  and model.  There are no arguments; instead, everything necessary    #
#  for a successful ETA forecast is specified and documented below.     #
#                                                                       #
#  Running this script assumes that 1) the raw GRIB data have been      #
#  processed into an ETA-ready format and are located in the            #
#  $ETA_DATA/eta_prep directory and 2) the "this_run" file, which       #
#  contains the names of files to be used in the forecast is located    #
#  in the $ETA_RUN directory.  Note that if the ETA_DOWNLOAD script     #
#  is run prior to the execution of the ETA_RUN script, this step has   #
#  already been done for you.                                           #
#                                                                       #
#  That's it. No need to manually recompile the source code each time   #
#  you change the number of model grid points. That job will be done    #
#  automatically by this script (see below). This is the one-stop       #
#  shopping script for all your ETA modeling needs. Simply configure    #
#  the model run by setting the various parameters in the first two     #
#  sections below.                                                      #  
#                                                                       #
#  Please send any comments, suggestions, or bug fixes to:              #
#  Robert.Rozumalski@noaa.gov                                           #
#                                                                       #
#  LOG:                                                                 #
#       R.Rozumalski - 03/00    Version 1.0 - Initial Release           #
#       R.Rozumalski - 07/00    Version 1.1 - Various bug enhancements  #
#       R.Rozumalski - 04/01    Version 2.0                             #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#          ETA RUN SCRIPT CONFIGURATION SECTION                         #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
#    AUTO_MAKE                                                          #
#                                                                       #
# 1. If any of the dimensions of the model grid array IM, JM, LM, or    #
#    LSM in the $ETA_HOME/src/config/dimensions.conf file do not agree  #
#    those defined below, then new executables for the model will be    #
#    recompiled.                                                        #
#                                                                       #
#    It is advised that you set AUTO_MAKE = yes, otherwise, if the grid #
#    dimensions below are not the same as the compiled model, this      #
#    script will terminate and you will need to manually edit the       #
#    dimensions.conf file and recompile.                                #
#                                                                       #
#########################################################################
set AUTO_MAKE = yes

#########################################################################
#    TERRAIN                                                            #
#                                                                       #
# 2. Set TERRAIN = yes if you want to extract the model terrain from    #
#    the topography dataset downloaded with the model. This data should #
#    be located in $ETA_DATA/topo. Only set TERRAIN = yes if you have   #
#    changed the grid configuration, changed the areal coverage of the  #
#    model domain, or have downloaded a new terrain dataset. You should #
#    set TERRAIN = no otherwise.                                        #
#                                                                       #
#########################################################################
set TERRAIN = yes

#########################################################################
#    PREPROCESS                                                         #
#                                                                       #
# 3. set PREPROCESS = yes if you want to create initial and boundary    #
#    condition files for your model run. The only reason for            #
#    PREPROCESS = no is if the model run crashed and you want to        #
#    restart it with the same initial and BC files.                     #
#                                                                       #
#########################################################################
set PREPROCESS = yes

#########################################################################
#    KEEP_RAW                                                           #
#                                                                       #
# 4. set KEEP_RAW = yes if you want to keep the ETA-ready data files    #
#    used by the preprocessor to create the initial and boundary        #
#    condition files.  These files are generated during execution of    #
#    the eta_download script and are no longer needed following a       #
#    successful completion of the model run.  The only reason why you   #
#    might want to retain these files is if you are running in          #
#    CASE_STUDY mode and don't want to reprocess these data from GRIB.  #
#    The default setting is KEEP_RAW = no.                              # 
#                                                                       #
#########################################################################
set KEEP_RAW = no

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#    ETA MODEL CONFIGURATION SECTION                                    #
#                                                                       #
#    The following sections define the user-controlled parameters for   #
#    running the workstation Eta model.  The first section contains     #
#    settings that should be modified by the user when setting up the   #
#    model for real-time or case study use. The latter sections         #
#    contain parameters that should be modified by the user only if     #
#    absolutely necessary.                                              #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#    SPECIAL NOTE ON BENCHMARKING                                       #
#                                                                       #
#    The purpose of running a benchmark case is to get a performance    #
#    appraisal of the model on your workstation.  You will also be able #
#    to compare your results to those from other systems. The results of#
#    these tests will eventually be made available on the soostrc web   #
#    site for others to peruse.                                         #
#                                                                       #
#    The eta_run.csh script comes set up to run the WS Eta benchmark    #
#    case.  TO RUN THE BENCHMARK CASE:                                  #
#                                                                       #
#    1) Download the benchmark data from the server:                    #
#       %   eta_download.csh   B                                        #
#                                                                       #
#    2) Run the model with the benchmark settings:                      #
#       %   eta_run.csh        B                                        #
#                                                                       #
#    To get an accurate appraisal of the model performance, you will    #
#    need to run the model from the command line without any additional #
#    non-essential processes running on your system. All that you need  #
#    be be concerned with is the total amount of time require to run    #
#    the WS Eta and not all the downloading and pre-processing stuff.   #
#    You do not need to convert the model data but feel free to to so   #
#    if you so desire.                                                  #
#                                                                       #
#    After running the case please send the following information to    #
#    the National SOO/STRC Coordinator: Robert.Rozumalski@noaa.gov      #
#                                                                       #
#    Machine platform:   (Ex: HP 715/64, INTEL )                        #
#    Processor speed:    (Ex: 64MHz, 800MHz)                            #
#    Operating System:   (Ex: HPUX 10.20, Redhat 6.0)                   #
#    Compiler            (Ex: HP, ABSOFT, PGF, SOOSAC binaries)         #
#    Machine Memory:     (Ex: 512mb )                                   #
#    Clock time:         (Ex 2h 16min)                                  #
#                        Simply use to start and stop time from the     #
#                        eta_run.csh script                             #
#                                                                       #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#    SPECIAL NOTE ON SELECTING A MODEL DOMAIN                           #
#                                                                       #
#    The following 3 sections define the coverage area of your          #
#    workstation ETA model domain.  However, due to the staggard grid   #
#    configuration, the intuitive relationship between a specified      #
#    number of grid points in the and the areal coverage is not valid.  #
#                                                                       #
#    Consequently, a utility program, ETAMAP, has been provided for you #
#    to get a better idea of your computational model domain, given     #
#    the 3 sets of values specified below. The program is located in    #
#    the $ETA_HOME/util directory and must be installed with your       #
#    nawips distribution. It is strongly suggested that you run etamap  #
#    before finalizing your domain. More information can be obtained    #
#    by typing "h etamap" at the GEMPAK-ETAMAP> prompt.                 #
#                                                                       #
#    A more crude program, get_area.exe, is also provided and is        #
#    located in the $WS_ETA/exe/$OS directory.                          #
#                                                                       #
#    BINARY DISTRIBUTION USERS:  Since you can not modify the number of #
#    grid points in your model domain, I have precompiled the binaries  #
#    so that the areal coverage of your model domain has a N-S/E-W      #
#    ratio (in degrees) of approximately 2/3. You may use etamap or the #
#    get_area.exe program to help select your domain; however, you will #
#    have to enter the appropriate values for the horizontal grid       #
#    points when prompted.                                              #
#                                                                       #
#    Note that you may still modify the horizontal resolution to any    #
#    desired value and place the computational domain wherever you like.#
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
#    MODEL_RUN  {HYDROSTATIC or NON-HYDROSTATIC}                        #
#                                                                       #
# 5. MODEL_RUN specifies whether the model run is HYDROSTATIC or        #
#    NON-HYDROSTATIC. It is suggested that you coinsider using the      #
#    NON-HYDROSTATIC option only when running with grid spacing of less #
#    than 10km (DLMD, DLHD < 0.068, see below).                         #
#                                                                       #
#            !!!!!!!!!!! CAVEAT EMPTOR !!!!!!!!!!!!!!                   #
#                                                                       #
#    Running the WS Eta in  NON-HYDROSTATIC mode GREATLY increases the  #
#    CPU (and clock) time.                                              #
#                                                                       #
#                                                                       #
#########################################################################
set MODEL_RUN = HYDROSTATIC

#########################################################################
#    V_COORD  {ETA or SIGMA}                                            #
#                                                                       #
# 6. The V_COORD setting allows the user to run the model with either   #
#    an ETA or SIGMA vertical coordinate.  The default setting is ETA.  #
#                                                                       #
#########################################################################
set V_COORD = SIGMA

#########################################################################
#    IM, JM, and LM                                                     #
#                                                                       #
# 7. IM, JM, and LM are the number of grid points over the              #
#    computational domain where:                                        #
#                                                                       #
#       IM is the number of grid points in the E-W (roughly) direction  #
#       JM is the number of grid points in the N-S (roughly) direction  #
#       LM is the number of grid points in vertical.                    #
#                                                                       #
#    Note that if these values must be the same as those used to compile#
#    the model executables ($ETA_HOME/src/config/dimensions.conf). If   #
#    are not, and AUTO_MAKE = yes, a new executable will be built and   #
#    the script will continue on its merry way.                         #
#                                                                       #
#    IMPORTANT:  The horizontal grid dimensions MUST be an ODD integer! #
#                                                                       #
#    In addition, there are only 4 possible vertical level              #
#    configurations (LM) available, either 38, 45, 50, or 60 levels.    #
#                                                                       #
#    BINARY DISTRIBUTION USERS: The IM and JM values must be set to the #
#    dimensions of your compiled distribution. LM should be 45.         #
#                                                                       #
#    BENCHMARK:  IM = 55, JM = 91, LM = 45                              #
#########################################################################
set IM = 55
set JM = 91
set LM = 45

#########################################################################
#    TLM0D and TPH0D                                                    #
#                                                                       #
# 8. TLM0D and TPH0D are the center longitude (E is positive) and       #
#    latitude (W is negative) of the computational domain. Note that    #
#    it is not necessary to to recompile the model if you change the    #
#    areal coverage of the model domain as long as the number of grid   #
#    points remains the same.                                           #
#                                                                       #
#    For a grid centered at 90 W, 35 N (approximately the central US)   #
#                                                                       #
#       TLM0D =  -90.0  Note the Negative for W                         #
#       TPH0D =   35.0                                                  #
#                                                                       #
#   BENCHMARK: TLM0D = -94.0, TPH0D =  41.7                             #
#########################################################################
set TLM0D = -94.0
set TPH0D =  41.7


#########################################################################
#    DLMD, DLHD                                                         #
#                                                                       #
# 9. DLMD and DLHD are the grid spacing of the computational model      #
#    domain in degrees latitude and longitude respectively.  A VERY     #
#    rough conversion to km over the central US would be:               #
#                                                                       #
#            km/150 = delta degrees                                     #
#                                                                       #
#    Thus from above:                                                   #
#                                                                       #
#            10km grid spacing: DLMD, DLHD = 0.067                      #
#            15km grid spacing: DLMD, DLHD = 0.098                      # 
#            32km grid spacing: DLMD, DLHD = 0.213                      #
#                                                                       #
#    You can use any values you want. Note that DLMD and DLHD should    #
#    be the same.                                                       #
#                                                                       #
#    BENCHMARK: DLMD = 0.098, DPHD = 0.098                              #
#########################################################################
set DLMD = 0.098
set DPHD = 0.098

#########################################################################
#    DT                                                                 #
#                                                                       #
#10. DT is the fundamental timestep of the model in seconds.  The       #
#    maxmum value of DT is governed by the horizontal resolution        #
#    specified above (DLMD and DPHD).  Selection of a timestep for a    #
#    model run is a balance between computational efficiency and        #
#    numerical stability.  A smaller timestep  provides greater         #
#    computational accuracy at the expense of longer execution time     #
#    for the forecast. Too large a  DT will result in numerical         #
#    instablities, which are manifested as large pressure depth changes #
#    in the model atmosphere. If this instability occurs during a       #
#    simulation the model will stop and the error message, "Big pd      #
#    change (mb)" will be sent to standard output. In addition, the DT  #
#    value MUST evenly divide into 3600s (1 hour).                      #
#                                                                       #
#    A rule of thumb for selection of DT. choose a value approximately  #
#    2x grid spacing, in km, that divides evenly into 3600. For the     #
#    above grid space values, the DT would be:                          #
#                                                                       #
#             10km:  DT = 20 (s)                                        #
#             15km:  DT = 30 (s)                                        #
#             32km:  DT = 72 (s)                                        #
#                                                                       #
#    BENCHMARK: DT = 30                                                 #
#########################################################################
set DT = 30

#########################################################################
#    FCST_LENGTH                                                        #
#                                                                       #
#11. FCST_LENGTH is the length of the model forecast in hours. Note     #
#    that the model will only be run out FCST_LENGTH hours, or to the   #
#    last available time of BC data as specified when downloading       #
#    grib data, which ever is less.  Thus, if FCST_LENGTH is set to     #
#    48 hours but you have only requested 24 hours worth of GRIB data,  #
#    then the model will only run for 24 hours.  Note that a WARNING    #
#    message will be generated if this should occur.                    #
#                                                                       #
#    BENCHMARK: FCST_LENGTH = 24                                        #
#########################################################################
set FCST_LENGTH = 24

#########################################################################
#    OUTPUT PRESSURE LEVELS                                             #
#                                                                       #
#12. There are 2 different methods by which you can specify the levels  #
#    output to the forecast files. You may either select the levels     #
#    individually ( method 1 ) or by specifying a range  and interval   #
#    ( method 2 ). All pressure are in millibars.                       #
#                                                                       #
#    METHOD 1:                                                          #
#                                                                       #
#    If you want to use method 1 for requesting output levels then set  #
#    P_METHOD = 1 and then specify the levels in OUT_PLEVS. For         #
#    Example:                                                           #
#                                                                       #
#        set P_METHOD = 1                                               #
#        set OUT_PLEVS = ( 100 200 300 400 500 600 700 850 925 1000 )   #
#                                                                       #
#                                                                       #
#    METHOD 2:                                                          #
#                                                                       #
#    OUT_PBOT, OUT_PTOP, and  OUT_PINT define the pressure levels (mb)  #
#    on which Eta forecast data will be written out for viewing.        #
#    OUT_PBOT is the bottom (i.e, closest to the surface), OUT_PTOP is  #
#    the top, and OUT_PINT is the interval between the bottom and top   #
#    pressure levels (mb).  For example, if you wish to view every      #
#    25 mb between 1000 and 150 mb (great for X-sections):              #
#                                                                       #
#        set  P_METHOD = 2                                              #
#        set  OUT_PBOT = 1000                                           #
#        set  OUT_PTOP = 150                                            #
#        set  OUT_PINT = 25                                             #
#                                                                       #
#    The total number of output levels is computed from:                #
#                                                                       #
#        ( ( OUT_PBOT - OUT_PTOP ) / OUT_PINT ) + 1                     #
#                                                                       #
#    The model will output data on 35 different pressure levels every   #
#    25 mb between 1000 and 150 mb.                                     #
#                                                                       #
#                                                                       #
#    NOTE:  If you are using precompiled binaries then the number of    #
#           output levels must be the same as specified by the LSM      #
#           parameter in $WS_ETA/src/config/dimensions.conf.            #
#           You are not limited in the location of the output levels,   #
#           only the number of levels.                                  #
#                                                                       #
#           If you somehow fail to count accurately and use an incorrect#
#           number of levels, this script will either recompile         #
#           the model package (If you have a F90 and ANSI-C compiler)   #
#           or terminate and prompt you to modify the number of levels. #
#                                                                       #
#    BINARY DISTRIBUTION USERS: You can modify this parameter, however, #
#    the total number of pressure levels, must be equal to 18 (the      #
#    compiled value).                                                   #
#                                                                       #
#    BENCHMARK: P_METHOD=2, OUT_PBOT=1000, OUT_PTOP=150, OUT_PINT=50    #
#########################################################################
set P_METHOD  = 2

# Use for method 1 only ( \ is for line continuation )
#
set OUT_PLEVS = ( 550 650 750 950 100 200 300 400 850 925 \
                  1000 500 600 700 150 250 350 450 )

# Use for method 2 only
#
set OUT_PBOT  = 1000
set OUT_PTOP  = 150
set OUT_PINT  = 50

#########################################################################
#    CUMULUS                                                            #
#                                                                       #
#13. CUMULUS defines the cumulus paramiterization scheme to use when    #
#    running the model.  Your choices are:                              #
#                                                                       #
#           CUMULUS = 0           No cumulus parameterization           #
#           CUMULUS = 1           Betts-Miller-Janic (BMJ) scheme       #
#           CUMULUS = 2           Kain-Fritch scheme                    #
#                                                                       #
#           Note that using option 0 will result in precipitation       #
#           generated by the grid-scale scheme only!                    #
#                                                                       #
#           BTW - you can compare the amount or precipitation generated #
#                 by the cumulus and grid-scale schemes by looking at   #
#                 the C??M and S??M ( M for milimeters, I for inches)   #
#                 variables in GEMPAK or GARP where ?? is the period    #
#                 (in hours) over which to accumulate the data.         #
#                                                                       #
#   BENCHMARK: CUMULUS = 2                                              #
#########################################################################
set CUMULUS = 2

#########################################################################
#    OUT_FREQ                                                           #
#                                                                       #
#14. OUT_FREQ specifies the frequency (in hours) with which to write    #
#    out the model forecast data. A value of 3 will cause the forecast  #
#    data to be written out every 3 hours beginning with the 0 hour     #
#    forecast.                                                          #
#                                                                       #
#    BENCHMARK: OUT_FREQ = 3                                            #
#########################################################################
set OUT_FREQ = 3

#########################################################################
#    ACUM_PRECIP                                                        #
#                                                                       #
#15. ACUM_PRECIP is the amount of time over which to accumulate         #
#    precipitation in the model forecast fields before being reset.     #
#    If you want 3 houly precipitation amounts then set ACUM_PRECIP = 3 #
#    If you are interested in accumulating the precipitation over the   #
#    entire model forecast length set ACUM_PRECIP = FCST_LENGTH and the #
#    script will figure out the rest.  Note that ACUM_PRECIP should be  #
#    greater than or equal to the value of OUT_FREQ. In this way the    #
#    precipitation totals in each of the output files are cumulative    #
#    precipitation from the previous output time. You can also set      #
#    ACUM_PRECIP = OUT_FREQ.                                            #
#                                                                       #
#    BENCHMARK: ACUM_PRECIP = $OUT_FREQ                                 #
#########################################################################
set ACUM_PRECIP = $OUT_FREQ

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#    END OF PRIMARY MODEL CONFIGURATION.  THERE IS NO NEED TO CHANGE    #
#    ANYTHING BELOW THIS LINE.                                          #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
#    WEIGHT                                                             #
#                                                                       #
#    WEIGHT determines the amount of horizontal diffusion to provide    #
#    during model execution for the suppression of gravity wave noise.  #
#    A good value for WEIGHT is 0.25.                                   #
#                                                                       #
#########################################################################
set WEIGHT = 0.25

#########################################################################
#    IDTAD                                                              #
#                                                                       #
#    IDTAD is the frequency of the advective timestep relative to the   #
#    fundamental timestep (DT) in the model. This is sometimes referred #
#    to as the "short timestep" and may vary depending upon the model.  #
#    For the Workstation Eta a good value is 2.                         #
#                                                                       #
#########################################################################
set IDTAD = 2


#########################################################################
#    SOIL                                                               #
#                                                                       #
#    SOIL defines the number of soil levels in the model. This value A  #
#    should be set to 4.                                                #
#                                                                       #
#########################################################################
set SOIL = 4

#########################################################################
#    LEVELS                                                             #
#                                                                       #
#    LEVELS defines the allowable vertical levels in the model.         #
#    Note that these must correspond with the configuration files in    #
#    ${ETA_HOME}/src/config/deta_files. currently there are 4 different #
#    vertical level configurations 38, 45, 50, and 60                   #
#                                                                       #
#    BINARY DISTRIBUTION USERS: You can not modify this parameter       #
#                                                                       #
#########################################################################
set LEVELS = ( 38 45 50 60 )

#########################################################################
#    PTOP                                                               #
#                                                                       #
#    PTOP define the top of the model atmosphere (in mb).  Currently,   #
#    there are only 2 choices, either 50 or 25 (mb).                    #
#                                                                       #
#    The reason for placing the model top well above the tropopause     #
#    is to reduce the reflection of vertically propagating gravity      #
#    waves off the model rigid lid.                                     #
#                                                                       #
#    It is advisable to set PTOP = 25 (mb)                              #
#                                                                       #
#    If you chose another pressure for PTOP (i.e, 50mb), you will have  #
#    to manually edit the $ETA_HOME/src/eta_prep/initbc/interp.f file   #
#    and recompile the model. pt (pressure top) is defined in a data    #
#    statement at the top of the subroutine vinterp.                    #
#                                                                       #
#    BINARY DISTRIBUTION USERS: You can not modify this parameter       #
#                                                                       #
#########################################################################
set PTOP = 25

#########################################################################
#    PLEVELS                                                            #
#                                                                       #
#    PLEVELS defines the allowable pressure levels (mb) for the top of  #
#    the model atmosphere.                                              #
#                                                                       #
#    Note that these must correspond with the configuration files in    #
#    ${ETA_HOME}/src/config/rad_files. currently there are 2 different  #
#    model top configurations 50 and 25 mb                              #
#                                                                       #
#    BINARY DISTRIBUTION USERS: You can not modify this parameter       #
#                                                                       #
#########################################################################
set PLEVELS = ( 25 50 )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                       #
#    THERE IS REALLY NO NEED TO CHANGE ANYTHING BELOW THIS LINE.        #
#                                                                       #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#########################################################################
#  Preliminary stuff                                                    #
#########################################################################
unalias rm
unalias cd

#########################################################################
#  Define the ETA_HOME directory again, just to make sure               #
#########################################################################

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

#########################################################################
#                                                                       #
# THE ARGUMENT "B" HAS BEEN PASSED INDICATING THAT THIS IS A BENCHMARK  #
# RUN. RESET ALL PARAMETERS TO THE BENCHMARK VALUES.                    #
#                                                                       #
######################################################################### 
#

if ( $1 == B || $1 == b ) then
    set AUTO_MAKE   = yes
    set TERRAIN     = yes
    set PREPROCESS  = yes
    set KEEP_RAW    = no
    set MODEL_RUN   = HYDROSTATIC
    set V_COORD     = ETA
    set IM          = 55
    set JM          = 91
    set LM          = 45
    set TLM0D       = -94.0
    set TPH0D       =  41.7
    set DLMD        = 0.098
    set DPHD        = 0.098
    set DT          = 30
    set FCST_LENGTH = 24
    set P_METHOD    = 2
    set OUT_PBOT    = 1000
    set OUT_PTOP    = 150
    set OUT_PINT    = 50
    set CUMULUS     = 2
    set OUT_FREQ    = 03
    set ACUM_PRECIP = $OUT_FREQ
endif


#########################################################################
#  DETERMINE THE FILENAMES OF THE ETA_READY DATA GENERATED BY THE
#  PREPROCESSOR.  ASSUME THAT THE FIRST DATA FILE FOUND IS THE FIRST
#  GUESS FIELDS AND THE OTHERS ARE BOUNDARY CONDITION DATA.
#
#  ALL THE DATA SHOULD BE LOCATED IN $ETA_PREP
#
onintr clean_prerun

find $ETA_TMP -name eta_lock -atime +0 -exec rm -f {} \; >& /dev/null
if ( -e $ETA_TMP/eta_lock ) then
   echo " "
   echo "ERROR:   The eta_run.csh program has detected a eta_lock"
   echo "         file in the $ETA_TMP directory. "
   echo "         Either a WS ETA run is currently running or a "
   echo "         previous run terminated abnormally.  If there is "
   echo "         no model currently running then delete the "
   echo "         $ETA_TMP/eta_lock file and try again."
   echo " "
   exit 20
endif

set AUTO_MAKE   = `echo $AUTO_MAKE   | tr '[A-Z]' '[a-z]'`
set TERRAIN     = `echo $TERRAIN     | tr '[A-Z]' '[a-z]'`
set PREPROCESS  = `echo $PREPROCESS  | tr '[A-Z]' '[a-z]'`
set KEEP_RAW    = `echo $KEEP_RAW    | tr '[A-Z]' '[a-z]'`
set V_COORD     = `echo $V_COORD     | tr '[a-z]' '[A-Z]'`

cd $ETA_RUN
echo " "
echo " "
echo "#############################################################"
echo "############### Let the ETA run Script Begin ################"
echo "###### Starting ETA_RUN - `date` ######"
echo " "

#  DETERMINE WHETHER THE SPECIFIED NUMBER OF VERTICAL LEVELS IS VALID
#
set GOOD_LEV = NO

foreach LEVEL ( $LEVELS )
    if ( $LEVEL == $LM ) set GOOD_LEV = yes
end

if ( $GOOD_LEV == NO ) then
    echo "ERROR: Inappropriate number of vertical levels: LM="$LM
    echo "       Select from: " $LEVELS
    echo " "
    exit 1
endif

#  DETERMINE WHETHER THE PRESSURE AT THE MODEL TOP IS VALID
#

set GOOD_LEV = NO

foreach PLEVEL ( $PLEVELS )
    if ( $PLEVEL == $PTOP ) set GOOD_LEV = yes
end

if ( $GOOD_LEV == NO ) then
    echo "ERROR: Inappropriate choice for model top: PTOP="$LM
    echo "       Select from: " $PLEVELS
    echo " "
    exit 1
endif

#  DETERMINE IF THE COMPILED MODEL GRID DIMENSIONS ARE THE SAME AS
#  THE USER REQUESTED GRID.  IF THEY ARE NOT, AND AUTO_MAKE = YES,
#  THEN RECOMPILE ALL THE EXECUTABLES.
#
set MAKE_NEW = no

set PARAMS = ( `grep PARAMETER $ETA_HOME/src/config/dimensions.conf | head -1`)
set PARAM  = ( `echo $PARAMS | cut -d"(" -f2 | cut -d")" -f1` )
set IMT    = ( `echo $PARAM  | cut -d"," -f1 | cut -d= -f2` )
set JMT    = ( `echo $PARAM  | cut -d"," -f2 | cut -d= -f2` )
set LMT    = ( `echo $PARAM  | cut -d"," -f3 | cut -d= -f2` )
set LSMT   = ( `echo $PARAM  | cut -d"," -f4 | cut -d= -f2` )

if ( $P_METHOD == 2 ) then
   @   LSM    = ( $OUT_PBOT - $OUT_PTOP ) / $OUT_PINT
   @   LSM    = ( $LSM + 1 )
else if ( $P_METHOD == 1 ) then
   set LSM = $#OUT_PLEVS
   if ( $LSM <= 0 ) then
      echo "OOPS:  You have not selected a valid method for outputting"
      echo "       pressure levels ( see item 10 )"
      echo " "
      exit 1
   endif
else
   echo "OOPS:  You have not selected a valid method for outputting"
   echo "       pressure levels ( see item 10 in eta_run.csh file )"
   echo " "
   exit 1
endif

if ( $IMT != $IM || $JMT != $JM || $LMT != $LM || $LSMT != $LSM ) then

   echo " "
   echo "WARNING:  Current compiled model dimensions are not "
   echo "          identical to user-specified grid dimensions:"
   echo " "
   echo "                             Model    New"
   echo "                        IM    "$IMT  "     "$IM 
   echo "                        JM    "$JMT  "     "$JM 
   echo "                        LM    "$LMT  "     "$LM 
   echo "                        LSM   "$LSMT "     "$LSM 
   echo " "

#  TEST TO SEE IF THERE IS A LOCKFILE IN THE EXECUTABLE DIRECTORY.
#  IF THIS IS TRUE THEN A BINARY DISTRIBUTION WAS INSTALLED AND
#  IT IS ASSUMED THAT THE WORKSTATION IS MISSING THE F90 and F77
#  COMPILERS.
#
   if ( -e $ETA_EXE/.lock ) then
 
      echo " "
      echo "SORRY:  A binary distribution of the workstation ETA"
      echo "        was installed on this system.  If you have"
      echo "	    the necessary F90 and ANSCI-C compilers to"
      echo "        build the workstation ETA, then remove the"
      echo "        $ETA_EXE/.lock file and execute this,"
      echo "        script again.  Otherwise, set the IM, JM, LM,"
      echo "        and LSM variables at the top of file to the"
      echo "        model values listed above."
      echo " "
      exit 99

   endif

   if ( $AUTO_MAKE == yes ) then
 
#     IF AUTO_MAKE = YES, THEN CREATE A NEW dimensions.conf FILE
#     AND RECOMPILE THE MODEL.  THE PREVIOUS FILE WILL BE RENAMED
#     dimensions.conf_prev.  DETAILS FROM THE MAKE ARE SAVED IN
#     THE $ETA_TMP/make.log FILE.
#
      set LWD = `pwd`
      set MAKE_NEW = yes
      set ETA_CONFIG = $ETA_HOME/src/config

      echo " "
      echo "  #########################################################"
      echo "  ##########  Recompiling Workstation ETA Model  ##########"
      echo "  ##### Compiling Began - `date` ####"
      echo " "

      mv ${ETA_CONFIG}/dimensions.conf  ${ETA_CONFIG}/dimensions.conf_prev
      echo "C-------------------------------------------------------------" > ${ETA_CONFIG}/dimp1.parm
      echo "C***  SET PRIMARY GRID DIMENSIONS AND PRESSURE OUTPUT LEVELS"  >> ${ETA_CONFIG}/dimp1.parm
      echo "      PARAMETER (IM=$IM,JM=$JM,LM=$LM,LSM=$LSM)"               >> ${ETA_CONFIG}/dimp1.parm
      cat  ${ETA_CONFIG}/dimp1.parm ${ETA_CONFIG}/dimp2.parm > ${ETA_CONFIG}/dimensions.conf

      echo "  Further info can be found in ${ETA_TMP}/make.log."
      cd $ETA_HOME; make clobber >& ${ETA_TMP}/make.log; make all >>& ${ETA_TMP}/make.log; cd ${LWD}

      echo " "
      echo "  ##### Compiling Ended - `date` ####"
      echo "  ########  Done recompiling Workstation ETA Model  #######"
      echo "  #########################################################"
      echo " "
   else
      "Edit ${ETA_CONFIG}/dimensions.conf and manually recompile the model."
   endif

endif

#  GET THE FILENAMES OF THE ETA-READY GRIDDED DATA THAT ARE
#  TO BE USED FOR THE INITIAL AND BOUNDARY CONDITIONS.
#
if ( ! -e $ETA_RUN/this_run ) then
   echo "  OOPS:  You need to provide the names of the eta-ready data"
   echo "         files in the $ETA_RUN/this_run file. "
   echo "         This file is generated by the eta_download.csh program. - EXIT"
   echo "    "
   exit 99
endif

set ETA_FILES = ( `cat $ETA_RUN/this_run` )

if ( $#ETA_FILES <= 1 ) then
   echo "  OOPS:  Only $#ETA_FILES file available in ${ETA_PREP}."
   echo "         There must be at least 2 files available. "
   echo "         Make sure that eta_download ran correctly. - EXIT" 
   echo " "
   exit 99
endif

foreach ETA_FILE ( $ETA_FILES )
   if ( ! -e ${ETA_PREP}/$ETA_FILE ) then
      echo " "
      echo "  ERROR:  ETA-ready file, ${ETA_PREP}/$ETA_FILE"
      echo "          was missing. Was it accidently erased? - EXIT"
      echo " "
      exit 99
   endif
end

#  DETERMINE THE DATES FOR THE MODEL RUN FROM THE FILENAMES
#  IN THE "this_run" FILE.
#
set MMDDHH = ( `echo $ETA_FILES[1] | cut -c3-8` )
set FF     = ( `echo $ETA_FILES[1] | cut -c9-11` )
set YY     = ( `echo $ETA_FILES[1] | cut -c1-2` )
set GDS    = ( `echo $ETA_FILES[1] | cut -d"." -f2` )
if ( $YY >  50 ) set YR = 19${YY}
if ( $YY <= 50 ) set YR = 20${YY}

#  CONVERT JULIAN DATE TO GREGORIAN DATE
#
if ( ! -e $ETA_EXE/initdate ) then
   echo " "
   echo "ERROR:  $ETA_EXE/initdate was not found."
   echo "        Go find it and try again."
   echo " "
   exit 99
endif
   
set YYYYMMDDHH = ( `$ETA_EXE/initdate ${YR}${MMDDHH} ${FF}` )
set YR = ( `echo $YYYYMMDDHH | cut -c1-4` )
set MO = ( `echo $YYYYMMDDHH | cut -c5-6` )
set DY = ( `echo $YYYYMMDDHH | cut -c7-8` )
set HR = ( `echo $YYYYMMDDHH | cut -c9-10` )
set YYMMDDHH = ( `echo $YYYYMMDDHH | cut -c3-10` )

set cnts = ( `echo $MO | wc` )
if ( $cnts[3] < 3 ) set MO = 0$MO

set cnts = ( `echo $YR | wc` )
if ( $cnts[3] < 3 ) set DY = 0$DY

#  DETERMINE THE MODEL CYCLE FROM THE FILENAME.
#
set CYCLE =  ( `echo $ETA_FILES[1] | cut -c7-8` )

#  COMPUTE THE FREQUENCY OF THE BC DATA FROM THE FILES IN THE
#  ${ETA_PREP} DIRECTORY.  IT IS ASSUMED THAT THESE
#  FILES ARE EVENLY SPACED IN TIME.
#
set INIT  =  ( `echo $ETA_FILES[1] | cut -c9-11` )
set FINL  =  ( `echo $ETA_FILES[$#ETA_FILES] | cut -c9-11` )
@   BCLEN =  ${FINL} - ${INIT}
@   TBOCO =  ${BCLEN} / ( $#ETA_FILES - 1 )
@   NHOUR =  $FINL - $INIT

# DETERMINE WHETHER THE USER WANTS THE ETA OR SIGMA
# VERTICAL COORDINATE SYSTEM.
#
if ( $V_COORD == SIGMA ) then
     set SIGMA = ".TRUE."
     set SPLINE = ".TRUE."
else if ( $V_COORD == ETA ) then
     set SIGMA = ".FALSE."
     set SPLINE = ".FALSE."
else
     echo " "
     echo "  WARNING: V_COORD should to be defined to either ETA or SIGMA."
     echo "           You have somehow mangled the spelling ( ${V_COORD} ),
     echo "           so you are going to get a ETA vercical coordinate"
     echo "           system whether you like it or not."
     echo " "
     set SIGMA = ".FALSE."
     set SPLINE = ".FALSE."
endif

if ( -e ${ETA_PREP}/rtg_sst_grb_0.5 ) then 
     set SST = HIRES
else if ( -e ${ETA_PREP}/gdas1.T00Z.sstgrb ) then
     set SST = LORES
else
     echo " "
     echo "ERROR  :   There is no SST data file found\! - EXIT"
     echo " "
     exit
endif

#  CREATE THE ETAIN FILE USED BY THE WORKSTATION ETA
#  FOR RUNNING THE SIMULATIONS.
#

if ( -f ETAIN ) rm -f ETAIN

echo "&MODEL_GRIDS"      >> ETAIN
echo " TLM0D=$TLM0D"     >> ETAIN
echo " TPH0D=$TPH0D"     >> ETAIN
echo " IM=$IM"           >> ETAIN
echo " JM=$JM"           >> ETAIN
echo " LM=$LM"           >> ETAIN
echo " PTINP=${PTOP}00." >> ETAIN
echo " DLMD=$DLMD"       >> ETAIN
echo " DPHD=$DPHD"       >> ETAIN
echo " DT=${DT}."        >> ETAIN
echo " IDTAD=$IDTAD"     >> ETAIN
echo " IMONTH=$MO"       >> ETAIN
echo " IDATE=$DY"        >> ETAIN
echo " IYEAR=$YR"        >> ETAIN
echo " ISTRTIM=$HR"      >> ETAIN
echo " NSOIL=$SOIL"      >> ETAIN
echo " NINIT=$#ETA_FILES">> ETAIN
set N = 1
foreach ETA_FILE ( $ETA_FILES )
   echo " INIT_IN(${N})='${ETA_PREP}/$ETA_FILE'" >> ETAIN
   @ N +=1
end
echo " INIT_GDSDIR='${ETA_PREP}/gdsinfo.${GDS}'" >> ETAIN
echo " INIT_OUT='$ETA_RUN'"               >> ETAIN
echo " TBOCO=${TBOCO}.0"                  >> ETAIN
echo " NHOUR=${NHOUR}"                    >> ETAIN
echo "&END"                               >> ETAIN
echo " "                                  >> ETAIN
echo "&SURFACE"                           >> ETAIN
echo " TOPO_IN='$ETA_DATA/topo/'"         >> ETAIN
echo " TOPO_OUT='$ETA_RUN/etatopo.dat'"   >> ETAIN
echo " GRIBSOIL=.TRUE."                   >> ETAIN
echo " seares=2."                         >> ETAIN 
echo "&END"                               >> ETAIN
echo " "                                  >> ETAIN
echo "&INIT_DIAG"                         >> ETAIN
echo " SIGMA=${SIGMA}"                    >> ETAIN
if ( $SST == HIRES ) then
   echo " HIRES=.TRUE."                   >> ETAIN
else
   echo " HIRES=.FALSE."                  >> ETAIN
endif
echo "&END"                               >> ETAIN

#  GENERATE SUMMARY OF MODEL RUN FOR USER
#
if ( $1 == 'b' || $1 == 'B' ) then
echo " "
echo "                    BENCHMARK RUN              "
endif
echo " "
echo "                    Model run Summary          "
echo " "
echo "    Model Configuration                        "
echo " "
echo "    Model Run:         $MODEL_RUN              "
echo " "
echo "    Grid Dimensions:   ${IM}x${JM}  $LM levels "
echo "    Vertical Coord:    $V_COORD               "
echo "    Model Top:         $PTOP mb                "
echo "    Grid Center:       $TPH0D,$TLM0D           "
echo "    Grid Spacing:      $DLMD Degrees           "
echo "    Model Timestep:    $DT    Seconds          "
echo "    BC Frequency:      $TBOCO     Hourly       "
echo "                                               "
echo "    Forecast Information                       "
echo "                                               "
echo "    Start Time:        $MO/$DY/$YR $HR UTC     "
echo "    Forecast Length:   $FCST_LENGTH    Hours   "
echo "    Model Output:      $OUT_FREQ     Hourly    "
echo "                       Every $OUT_PINT mb from $OUT_PBOT to $OUT_PTOP mb"
echo "    Precip Accum:      $ACUM_PRECIP     Hourly "

                     set CUM = "No cumulus parameterization"
if ( $CUMULUS == 1 ) set CUM = "Betts-Miller-Janic scheme"
if ( $CUMULUS == 2 ) set CUM = "Kain-Fritsch scheme"

echo "    Cumulus Scheme:    $CUM                    "
if ( $SST == HIRES ) echo "    SST Dataset:       0.5   Degree hires"
if ( $SST != HIRES ) echo "    SST Dataset:       1.0   Degree lowres"
echo "                                               "
echo " "

# --------------------------------------------------
#   If TERRAIN = yes or if the model was recompiled,
#   generate a new terrain dataset.
# --------------------------------------------------
#

#  IF TERRAIN FILES ARE MISSING THEN SET TERRAIN = yes
#

if ( ! -e ${ETA_RUN}/ZEFF )        set TERRAIN = yes
if ( ! -e ${ETA_RUN}/etatopo.dat ) set TERRAIN = yes

if ( $TERRAIN == yes || $MAKE_NEW == yes ) then

#  MAKE SURE THAT THE etatopo.exe FILE EXISTS
#
    if ( ! -e $ETA_EXE/etatopo.exe ) then
       echo " "
       echo "ERROR:  $ETA_EXE/etatopo.exe was not found."
       echo "        Go find it and try again. "
       echo " " 
       echo "EXIT at `date`."
       echo " "
       exit 2
    endif

#   MAKE LINKS TO THE VARIOUS LAND-SEA MASKS.  THE FOLLOWING IS FOR
#   RUNNING THE MODEL OVER NORTH AMERICA.
#
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/US_2m_slm.ieee US_2m_slm.ieee

#   UNCOMMENT THE FOLOWING LINES IF YOU ARE RUNNING THE MODEL OVER
#   A REGION  OTHER THAN NORTH AMERICA.
#
#   ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/global_4m.ieee global_4m.ieee
#   ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/global_8m.ieee global_8m.ieee

#   RUN THE TERRAIN EXECUTION PROGRAM
#
    echo " "
    echo "  #########################################################"
    echo "  ##########  Creating the ETA terrain data file  #########"
    echo "  #### Running terrain  - `date` ####"
    echo " "
    $ETA_EXE/etatopo.exe >& $ETA_TMP/eta_terrain.log

    grep "STOP_TERRAIN" $ETA_TMP/eta_terrain.log >& /dev/null
    if ( $status != 0 ) then
       echo "  BUMMER: Terrain generation failed for some yet unknown reason."
       echo "          Here are the last few lines of the eta_terrain.log file:"
       echo " "     
       tail -8       $ETA_TMP/eta_terrain.log
       echo " "
#      exit 97
    endif

    echo "  ### Terrain completed - `date` ####"
    echo "  #######  Done creating the ETA terrain data file  #######"
    echo "  #########################################################"
    echo " "
    echo "  Output is located in $ETA_TMP/eta_terrain.log"
    
 
#   CLEAN UP AND REMOVE LINKS
#
    if ( -e US_2m_slm.ieee ) rm -f US_2m_slm.ieee
    if ( -e global_4m.ieee ) rm -f global_4m.ieee
    if ( -e global_8m.ieee ) rm -f global_8m.ieee

endif


############################################################################
#                      EXECUTE THE PREPROCESSOR                            #
#                                                                          #
############################################################################
#
# --------------------------------------------------
#   If PREPROCESS = yes, set up and run preprocessor
# --------------------------------------------------
#
if ( $PREPROCESS == yes ) then

#   MAKE SURE THAT THE initbc.exe FILE EXISTS
#
    if ( ! -e $ETA_EXE/initbc.exe ) then
       echo " "
       echo "  ERROR:  $ETA_EXE/initbc.exe was not found."
       echo "          Go find it and try again. "
       echo "  EXIT at `date`."
       echo " "
       exit 2
    endif

#   REMOVE ANY EXISTING INITIAL AND BOUNDARY CONDITION FILES
#   THAT ARE IN THE RUN-TIME DIRECTORY.
#
#   IF YOU WANT TO KEEP THESE FILES THEN COMMENT OUT THIS LINE.
#
    if ( -e bndy.file ) rm -f bndy.file >& /dev/null
    if ( -e init.file ) rm -f init.file >& /dev/null

    ls init.???????? >& /dev/null
    if ( $status == 0 ) rm -f init.????????
    
    if ( $ETA_OS == hpux ) then
        set head = ftn
    else
        set head = fort.
    endif

#   CREATE LINKS FOR SURFACE FILES
#
    ln -s -f deta                                     ${head}16
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/alb1_ieee      ${head}21
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/alb2_ieee      ${head}22
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/alb3_ieee      ${head}23
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/alb4_ieee      ${head}24
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/ivgtyp_1d_ieee ${head}30
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/isltyp_1d_ieee ${head}31
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/islope_1d_ieee ${head}32
    ln -s -f ${ETA_RUN}/sstgrb                        ${head}39
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/rfusaflw_ieee  ${head}42
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/imsmask.ascii  ${head}43

    ln -s -f ${ETA_DATA}/const/deta_files/${ETA_OS}/deta_${LM} deta
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/alb1_ieee      alb1_ieee
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/alb2_ieee      alb2_ieee
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/alb3_ieee      alb3_ieee
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/alb4_ieee      alb4_ieee
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/ivgtyp_1d_ieee ivgtyp_1d_ieee
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/isltyp_1d_ieee isltyp_1d_ieee
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/islope_1d_ieee islope_1d_ieee

    if ( $SST == HIRES ) then
       ln -s -f ${ETA_PREP}/rtg_sst_grb_0.5           sstgrb
    else 
       ln -s -f ${ETA_PREP}/gdas1.T00Z.sstgrb         sstgrb
    endif   

    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/rfusaflw_ieee  rfusaflw_ieee
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/imsmask.ascii  imsmask.ascii
    ln -s -f ${ETA_PREP}/snowdepth.grb                snowdepth.grb
    ln -s -f ${ETA_PREP}/imssnow.grb                  imssnow.grb
    ln -s -f ${ETA_DATA}/sfc/${ETA_OS}/veg.eta.grb    veg.eta.grb



    echo " "
    echo "  #########################################################"
    echo "  ############  Running the ETA preprocessor  #############"
    echo "  #  Run Preprocessor Run - `date`  #"
    echo " "

    cat ETAIN             >& $ETA_TMP/eta_initbc.log
    $ETA_EXE/initbc.exe  >>& $ETA_TMP/eta_initbc.log

    grep "STOP_PREP" $ETA_TMP/eta_initbc.log >& /dev/null
    if ( $status != 0 ) then
       echo "BUMMER: The Eta Preprocessor failed for some yet unknown reason."
       echo " "
       echo "        Here are the last few lines of the eta_initbc.log file:"
       echo " "    
       tail -8       $ETA_TMP/eta_initbc.log
       echo " "
       exit 98
    endif

    echo "  # Preprocessor finished - `date`  #"
    echo "  #########  Done Running the ETA preprocessor  ###########"
    echo "  #########################################################"
    echo " "
    echo "  Output is located in $ETA_TMP/eta_initbc.log"
    echo " "

#   CLEAN UP LINKS TO FILES
#
    rm -f *ieee *.grb sstgrb* imsmask.ascii ${head}*
    rm -f preproc.*  deta
endif

# --------------------------------------------------
#   If everything has run successfully thus far 
#   Start the model.
# --------------------------------------------------
#
onintr clean_run

# Determine whether the user wanted a hydrostatic or non-hydrostatic
# model run.

if ( $MODEL_RUN == HYDROSTATIC ) then
     set HYDRO = ".TRUE."
else if ( $MODEL_RUN == NON-HYDROSTATIC ) then
     set HYDRO = ".FALSE."
else
     echo " "
     echo "  WARNING: MODEL_RUN should to be defined to either HYDROSTATIC"
     echo "           or NON-HYDROSTATIC. You have somehow mangled the spelling"
     echo "           ( ${MODEL_RUN} ), so you are going to get a HYDROSTATIC"
     echo "           forecast whether you like it or not."
     echo " "
     set HYDRO = ".TRUE."
endif

if ( $MODEL_RUN == HYDROSTATIC &&  $V_COORD == ETA )  then
   set MODEL_EXE = $ETA_EXE/eta_hy.exe
else
   set MODEL_EXE = $ETA_EXE/eta_nh.exe
endif

#   MAKE SURE THAT THE ETA EXECUTABLE EXISTS
#

if ( ! -e $MODEL_EXE ) then
    echo " "
    echo "  ERROR:  $MODEL_EXE was not found."
    echo "          Go find it and try again. "
    echo "  EXIT at `date`."
    echo " "
#   exit 2
endif

if ( ${BCLEN} < $FCST_LENGTH ) then
    echo " "
    echo "  WARNING: The value of FCST_LENGTH ($FCST_LENGTH hours) extends beyond"
    echo "           the available boundary condition data ($BCLEN hours)."
    echo "           The model forecast will only be $BCLEN hours."
    echo " "
    set FCST_LENGTH = ${BCLEN}
else if ( ${FCST_LENGTH} > ${BCLEN} ) then
    echo " "
    echo "  WARNING: The period BC files ($BCLEN hours) exceeds that of FCST_LENGTH "
    echo "           ($FCST_LENGTH hours). The model will only be run out $FCST_LENGTH hours."
    echo " "
endif

if ( $ACUM_PRECIP == OUT_FREQ ) then
   set ACUM_PRECIP = $OUT_FREQ
else if ( $ACUM_PRECIP == FCST_LENGTH ) then
   set ACUM_PRECIP = $FCST_LENGTH
endif

# Compute the value of NMAP, which is the number of 
# output file times.
#
@ NMAP = ( $FCST_LENGTH / $OUT_FREQ ) + 1

# Compute NPHS, the frequency, in fundamental timesteps,
# of calls to the physics package.  The model physics are
# generally run less frequently with increasing resolution.
# a general rule of thumb for the ETA Model is to set
# NPHS = 600/DT.
#
@ NPHS = ( 600 / $DT )

# NCNVC is the frequency, in model timesteps, of calls
# to the cumulus parameterization scheme.  It should 
# be the same number as NPHS.
#
set NCNVC = $NPHS

# NRADSH and NRADLH are the interval in hours between
# calls to the long and shortwave radiation schemes.
# These should be set to 1 and 2 respectively,
#
set NRADSH = 1
set NRADLH = 2

# NTDDMP if the frequency in model timesteps to apply
# the divergency damping. Typically set to 1.
#
set NTDDMP = 1

# TPREC, THEAT, TCLOD, TRDSW, TRDLW, and TSRFC are
# the period, in hours, over which to accumulate
# the precipitation, average latent heating, average
# cloud fractions, shortwave radiation, longwave
# radiation, and surface fluxes respectively for
# output.  They should be the same although they
# don't have to be.  These values are set above
# as OUT_FREQ.
#
set TPREC = $ACUM_PRECIP
set THEAT = $ACUM_PRECIP
set TCLOD = $ACUM_PRECIP
set TRDSW = $ACUM_PRECIP
set TRDLW = $ACUM_PRECIP
set TSRFC = $ACUM_PRECIP

#  WRITE ALL THE INPUT INFORMATION TO fcstdata. THIS FILE
#  IS USED BY THE MODEL FOR ALL THE CONFIGURABLE PARAMETERS.
#
if ( -e fcstdata ) rm -f fcstdata

echo "&FCSTDATA"                       >> fcstdata
echo " TSTART  = 00.0,"                >> fcstdata
echo " TEND    = ${FCST_LENGTH}.0,"    >> fcstdata
echo " TCP     = 99.0,"                >> fcstdata
echo " RESTRT  =.FALSE.,"              >> fcstdata
echo " SINGLRST=.TRUE.,"               >> fcstdata
echo " SUBPOST =.FALSE.,"              >> fcstdata
echo " NMAP    = ${NMAP},"             >> fcstdata
echo " TSHDE   = 00.0,"                >> fcstdata

set OT = 0
while ( ${OT} < ${FCST_LENGTH} )
   @ OT = $OUT_FREQ + $OT
   if ( $OT < 10 ) set OT = 0${OT}
   echo "           ${OT}.0,"          >> fcstdata
end

if ( $P_METHOD == 2 ) then
   set OL = ${OUT_PTOP}
   echo " SPL     = ${OUT_PTOP}00.,"       >> fcstdata
   set OL = ${OUT_PTOP}
   while ( ${OL} < ${OUT_PBOT} )
      @ OL = ${OL} + ${OUT_PINT}
      echo "           ${OL}00.,"         >> fcstdata
   end

else if ( $P_METHOD == 1 ) then

#  Reorganize levels from top [1] to bottom [N]
#
   set I = 1

   set NEW_PLEVS = `echo $OUT_PLEVS`

   while ( $I <= $#OUT_PLEVS )
       set N = 1
       set J = 1
       while ( $J <= $#OUT_PLEVS )
           if ( $OUT_PLEVS[$I] > $OUT_PLEVS[$J] )  @ N = $N + 1
           @ J = $J + 1
       end
       set NEW_PLEVS[$N] = $OUT_PLEVS[$I]
       @ I = $I + 1
   end

   set OUT_PLEVS = `echo $NEW_PLEVS`

   foreach OL ( $OUT_PLEVS )
      if ( $OL == $OUT_PLEVS[1] ) then
         echo " SPL     = ${OL}00.,"      >> fcstdata
      else
         echo "           ${OL}00.,"      >> fcstdata 
      endif
   end
endif

echo " NPHS   = $NPHS,"                >> fcstdata
echo " NCNVC  = $NCNVC,"               >> fcstdata
echo " NRADSH = $NRADSH,"              >> fcstdata
echo " NRADLH = $NRADLH,"              >> fcstdata
echo " NTDDMP = $NTDDMP,"              >> fcstdata
echo " TPREC  = ${TPREC}.0,"           >> fcstdata
echo " THEAT  = ${THEAT}.0,"           >> fcstdata
echo " TCLOD  = ${TCLOD}.0,"           >> fcstdata
echo " TRDSW  = ${TRDSW}.0,"           >> fcstdata
echo " TRDLW  = ${TRDLW}.0,"           >> fcstdata
echo " TSRFC  = ${TSRFC}.0,"           >> fcstdata
echo " NEST   =.FALSE.,"               >> fcstdata
echo " HYDRO  = ${HYDRO},"             >> fcstdata
echo " SPLINE = ${SPLINE},"            >> fcstdata
echo " ICUMULUS = ${CUMULUS}"          >> fcstdata
echo "&END"                            >> fcstdata

ln -s -f ${ETA_DATA}/const/rad_files/${ETA_OS}/co2.${LM}_${PTOP}mb  co2.dat
ln -s -f fcstdata                            ${head}11
ln -s -f cnst.file                           ${head}12
ln -s -f init.file                           ${head}13
ln -s -f co2.dat                             ${head}14
ln -s -f bndy.file                           ${head}16
ln -s -f ZEFF                                ${head}22
ln -s -f init.file			     init.${YYMMDDHH}
ln -s -f ${ETA_DATA}/const/solar solar
ln -s -f ${ETA_DATA}/const/bcexdata bcexdata

#   EXECUTE MODEL
#

touch $ETA_TMP/eta_lock

if (  $1 == A ) then
   echo "  Auto conversion of model output is ON"
   $ETA_SCRIPTS/eta_autoconvert.csh &
endif

if ( -e $ETA_TMP/eta_model.log ) rm -f $ETA_TMP/eta_model.log
echo " "
echo "  #########################################################"
echo "  ################  Now You're Modeling   #################"
echo "  ###### GO ETA GO\! - `date` ########"
echo " "
echo "  To watch the model Run - 'tail -f $ETA_TMP/eta_model.log' "
$MODEL_EXE >& $ETA_TMP/eta_model.log
echo " "
echo "  # ETA FORECAST COMPLETED - `date` #"
echo "  ####### Run eta_convert.csh to convert your data ########"
echo "  #########################################################"
echo " "

if ( -e $ETA_TMP/eta_lock ) rm -f $ETA_TMP/eta_lock

#   DETERMINE WHETHER THE RUN WAS SUCCESSFUL. IF YES, THEN
#   EXIT WITH STATUS OF 0. IF NOT EXIT WITH 98.
#

grep STOP_ETA $ETA_TMP/eta_model.log >& /dev/null
if ( $status == 0 ) then
   echo "  DONE\! - The WS Eta run was successful."
   echo " "
 
#  DELETE UNNECESSARY FILES.
#
   rm -f co2.dat solar bcexdata rdtnd* >& /dev/null

   echo "  Removing original preprocessor files from $ETA_PREP"
   foreach file ( $ETA_FILES )
      rm -f $ETA_PREP/${file}
   end
   if ( -e $ETA_PREP/gdas1.T00Z.sstgrb.index ) rm -f $ETA_PREP/gdas1.T00Z.sstgrb.index
   if ( -e $ETA_PREP/gdas1.T00Z.sstgrb       ) rm -f $ETA_PREP/gdas1.T00Z.sstgrb
   if ( -e $ETA_PREP/rtg_sst_grb_0.5         ) rm -f $ETA_PREP/rtg_sst_grb_0.5
   if ( -e $ETA_PREP/snowdepth.grb           ) rm -f $ETA_PREP/snowdepth.grb
   if ( -e $ETA_PREP/imssnow.grb             ) rm -f $ETA_PREP/imssnow.grb
   if ( -e $ETA_PREP/gdsinfo.*               ) rm -f $ETA_PREP/gdsinfo.*
   echo " "
   set exit_status = 0
else
   echo "  ERROR: The WS ETA did NOT run to completion - BUMMER..."
   echo " "
   set exit_status = 98
endif

echo " "
echo "############### Let the ETA run Script Ended ################"
echo "####### Ending ETA_RUN - `date` #######"
echo "#############################################################"
echo " "
exit $exit_status
 

clean_prerun:

   echo " "
   echo "  Cleaning up after interupt."
   echo " "
   rm -f $ETA_TMP/terrain.log    >& /dev/null
   rm -f ETAIN US_2m_slm.ieee global_4m.ieee global_8m.ieee >& /dev/null
   if ( -e $ETA_TMP/eta_lock ) rm -f $ETA_TMP/eta_lock
   exit 0

clean_run:

   echo " "
   echo "  Cleaning up after interupt."
   echo " "
   if ( -e $ETA_TMP/eta_lock ) rm -f $ETA_TMP/eta_lock
   if ( -e $ETA_TMP/eta_model.log ) rm -f $ETA_TMP/eta_model.log
   rm -f rm -f co2.dat solar bcexdata rdtnd*    >& /dev/null
   rm -f $ETA_TMP/terrain.log                   >& /dev/null
   rm -f ETAIN US_2m_slm.ieee global_4m.ieee global_8m.ieee >& /dev/null
   rm -f ${head}* >& /dev/null
   exit 0
