####################################################################################################
#
#  dc2metcsvpt.R
#
#  Author:
#    Melannie Hartman
#    melannie@ucar.edu
#    May 7, 2018
#
#  Description:
#    Read DayCent output files (psyn.out, soiltavg.out, and vswc.out), summarize
#    output, and write results to met.csv file. This met.csv file can be converted
#    to a point-level met.nc file for the testbed using csv2nc.R.
#
#    When computing the weighted average of soil temperture and soil moisture
#    this script assumes a predefined soil layer structure for both DayCent and CASA.
#    If either soil structure changes, code modifications will be needed.
#
#    This script also ignores day 366 from DayCent output since the CASA testbed files
#    always have 365 days per year.
#
#    Daily N deposition is set to zero for now.  This input is not important to the testbed
#    yet since the testbed only runs in C-only mode.
#
#    Frozen soil water is se to zero because this is not known in DayCent.
#
#       
#  Output file format (met.csv):
#   #   Variable                  Description
#  --   ------------------        ----------------------------------------------------------------
#   1   calendar_year             Calendar year (4 digits)
#   2   doy                       Day of year (1-366)
#   3   Tmax (degC)               Mean daily air temperature (deg C)
#   4   Tmin (degC)               Mean daily air temperature (deg C)
#   5   Precip (cm)               Daily precipitation (cm)
#   6   gpp (gC/m2/dy)            Gross Primary Production (gC/m2/day)
#   7   tsoil_1 (deg C)           Mean daily soil temperature 0-2.2 cm (deg C)
#   8   tsoil_2 (deg C)           Mean daily soil temperature 2.2-8.0 cm (deg C)
#   9   tsoil_3 (deg C)           Mean daily soil temperature 8.0-23.4 cm (deg C)
#  10   tsoil_4 (deg C)           Mean daily soil temperature 23.4-64.3 cm (deg C)
#  11   tsoil_5 (deg C)           Mean daily soil temperature 64.3 to 172.8 cm (deg C)
#  12   tsoil_6 (deg C)           Mean daily soil temperature 172.8 to 460 cm (deg C)
#  13   vswc_liq_1 (m3/m3)        Volumetric soil liquid water content 0-2.2 cm (0.0-1.0)
#  14   vswc_liq_2 (m3/m3)        Volumetric soil liquid water content 2.2-8.0 cm (0.0-1.0)
#  15   vswc_liq_3 (m3/m3)        Volumetric soil liquid water content 8.0-23.4 cm (0.0-1.0)
#  16   vswc_liq_4 (m3/m3)        Volumetric soil liquid water content 23.4-64.3 cm (0.0-1.0)
#  17   vswc_liq_5 (m3/m3)        Volumetric soil liquid water content 64.3 to 172.8 cm (0.0-1.0)
#  18   vswc_liq_6 (m3/m3)        Volumetric soil liquid water content 172.8 to 460 cm (0.0-1.0)
#  19   vswc_frzn_1 (m3/m3)       Volumetric soil frozen water content 0-2.2 cm (0.0-1.0)
#  20   vswc_frzn_2 (m3/m3)       Volumetric soil frozen water content 2.2-8.0 cm (0.0-1.0)
#  21   vswc_frzn_3 (m3/m3)       Volumetric soil frozen water content 8.0-23.4 cm (0.0-1.0)
#  22   vswc_frzn_4 (m3/m3)       Volumetric soil frozen water content 23.4-64.3 cm (0.0-1.0)
#  23   vswc_frzn_5 (m3/m3)       Volumetric soil frozen water content 64.3 to 172.8 cm (0.0-1.0)
#  24   vswc_frzn_6 (m3/m3)       Volumetric soil frozen water content 172.8 to 460 cm (0.0-1.0)
#  25   Ndep                      Atmospheric N deposition (gN/m2/day)
#
####################################################################################################

rm(list=ls())    # Clear memory

workdir <<- "E:/dev/NCAR/LTER/CPER/DayCent_UV/"
setwd(workdir)

inputPsynFileName = paste(workdir, "psyn.out", sep="")
inputSoilTavgFileName = paste(workdir, "soiltavg.out", sep="")
inputVSWCFileName = paste(workdir, "vswc.out", sep="")

psyn = read.table(file=inputPsynFileName, sep="", header=TRUE)
soiltavgOrig = read.table(file=inputSoilTavgFileName, sep="", header=FALSE)
vswcOrig = read.table(file=inputVSWCFileName, sep="", header=FALSE)

outputFileName = paste(workdir, "met.csv", sep="")
OutHdr <- c("calendar_year","doy","Tmax(degC)","Tmin(degC)","precip(cm/dy","gpp(gC/m2/dy)",
             "Tsoil_1(deg C)","Tsoil_2(deg C)","Tsoil_3(deg C)","Tsoil_4(deg C)","Tsoil_5(deg C)","Tsoil_6(deg C)",
             "vswc_liq_1(m3/m3)","vswc_liq_2(m3/m3)","vswc_liq_3(m3/m3)","vswc_liq_4(m3/m3)","vswc_liq_5(m3/m3)","vswc_liq_6(m3/m3)",
             "vswc_frzn_1(m3/m3)","vswc_frzn_2(m3/m3)","vswc_frzn_3(m3/m3)","vswc_frzn_4(m3/m3)","vswc_frzn_5(m3/m3)","vswc_frzn_6(m3/m3)",
             "Ndep(gN/m2/dy)")

# ATTENTION: If either DayCent or CASA layer structure changes, these weighted averages will need to be updated!

# Standard DayCent soil thicknesses for top 14 soil layers (cm) (210 cm depth maximum)
#   Added layer 15 (250cm) to make up the difference between deepest DayCent layer and deepest CASA layer.
#   The site may not have 14 soil layers. Extra layers in this list are OK.
dcsoilthickness = c(2, 3, 5, 10, 10, 15, 15, 15, 15, 15, 15, 30, 30, 30, 250)

# Standard CASACNP soil thicknesses (cm)
casasoilthickness = c(2.2, 5.8, 15.4, 40.9, 108.5, 287.2)

# These minimum and maximum soil layer depths are not currently being used 
# by the script. However, if soil layer structure needs to be flexible these 
# calculations will be helpful.
dcdepthmin <- array(data = 0.0, dim = c(length(dcsoilthickness)))
dcdepthmax <- array(data = 0.0, dim = c(length(dcsoilthickness)))
dcdepthmin[1] = 0
dcdepthmax[1] = dcsoilthickness[1]
for (lyr in 2:length(dcsoilthickness))
{
    dcdepthmin[lyr] = dcdepthmin[lyr-1] + dcsoilthickness[lyr-1]
    dcdepthmax[lyr] = dcdepthmax[lyr-1] + dcsoilthickness[lyr]
}
casadepthmin<- array(data = 0.0, dim = c(length(casasoilthickness)))
casadepthmax<- array(data = 0.0, dim = c(length(casasoilthickness)))
casadepthmin[1] = 0
casadepthmax[1] = casasoilthickness[1]
for (lyr in 2:length(casasoilthickness))
{
    casadepthmin[lyr] = casadepthmin[lyr-1] + casasoilthickness[lyr-1]
    casadepthmax[lyr] = casadepthmax[lyr-1] + casasoilthickness[lyr]
}

# Important columns in output file
colOutYr = 1
colOutDOY = 2
colOutTmax = 3
colOutTmin = 4
colOutPPT = 5
colOutGPP = 6
colOutTsoil1 = 7
colOutVSWC1 = 13
colOutFrznVSWC1 = 19
colOutNdep = 25

maxDays <- length(psyn[,1])                                # Number of daily values in DayCent input files
maxDays365 <- length(psyn[psyn[,colOutDOY]< 366,2])        # Number of days excluding day 366
maxCols <- length(OutHdr)                                  # Number of columns in the output file
outputSummary <- array(data = 0.0, dim = c(maxDays365, maxCols)) # Output excludes day 366
colnames(outputSummary) = OutHdr 

nDayCentLyrs <- length(vswcOrig[1,])-2  # The first two columns are time and DOY, the remaining are soil layers.
maxDayCentLyrs = length(dcsoilthickness)
soiltavg <- array(data = 0.0, dim = c(maxDays, maxDayCentLyrs))
vswc <- array(data = 0.0, dim = c(maxDays, maxDayCentLyrs))

# The DayCent soil profile (max 210 cm) does not go as deep as the CASA soil profile (460cm).
# Copy values from the DayCent files into the new data structure.  
# Use the bottom DayCent layer to fill in missing values in the profile. 

for (ilyr in 1:nDayCentLyrs)
{
    soiltavg[,ilyr] = soiltavgOrig[,ilyr+2]
    vswc[,ilyr] = vswcOrig[,ilyr+2]
}
# For missing layers at the bottom of the profile, fill in with bottom DayCent layer value.
for (ilyr in (nDayCentLyrs+1):maxDayCentLyrs)
{
    soiltavg[,ilyr] = soiltavgOrig[,nDayCentLyrs+2]
    vswc[,ilyr] = vswcOrig[,nDayCentLyrs+2]
}

# Important columns in input files
# psyn.out
colInTime = 1
colInDOY = 2
colInTmax = 4
colInTmin = 3
colInPPT = 6
colInGPPcrp = 19   # GPP crop
colInGPPfor = 26   # GPP forest
# soiltavg.out and vswc.out
colIn2Time = 1
colIn2DOY = 2
colInLyr1 = 3
colInLyrN = nDayCentLyrs+2

# Copy netcdf input files into .csv outputSummay table structure
# Do not write day 366 to the output file. 
#   I am assuming this is OK, even if there is a precipitation event on day 366, 
#   because the testbed does not use precipitation.
 
idy2 = 0
for (idy in 1:maxDays)
{
    if (psyn[idy,colInDOY] < 366)
    {
        idy2 = idy2 + 1
        outputSummary[idy2,colOutYr] = as.integer(psyn[idy,colInTime])
        outputSummary[idy2,colOutDOY] = psyn[idy,colInDOY]
        outputSummary[idy2,colOutTmax] = psyn[idy,colInTmax]
        outputSummary[idy2,colOutTmin] = psyn[idy,colInTmin] 
        outputSummary[idy2,colOutPPT] = psyn[idy,colInPPT] 
        outputSummary[idy2,colOutGPP] = psyn[idy,colInGPPcrp] + psyn[idy,colInGPPfor] 
        outputSummary[idy2,colOutNdep] = 0.0  # ATTENTION: retrieve this N deposition value

        # If either DayCent or CASA layer structure changes, these weighted averages will need to be updated.

        # (2*lyr1 + 0.2*lyr2)/2.2
        # (2.8*lyr2 + 3*lyr3)/5.8
        # (2*lyr3 + 10*lyr4 + 3.4*lyr5)/15.4
        # (6.6*lyr5 + 15*lyr6 + 15*lyr7 + 4.3*lyr8)/40.9
        # (10.7*lyr8 + 15*lyr9 + 15*lyr10 + 15*lyr11 + 30*lyr12 + 22.8*lyr13)/108.5
        # (7.2*lyr13 + 30*lyr14 + 250*lyr15)

        outputSummary[idy2,colOutTsoil1+0]    = (2.0*soiltavg[idy,1] + 0.2*soiltavg[idy,2]) / casasoilthickness[1]
        outputSummary[idy2,colOutVSWC1+0]     = (2.0*vswc[idy,1]     + 0.2*vswc[idy,2])     / casasoilthickness[1]
        outputSummary[idy2,colOutFrznVSWC1+0] = 0.0 

        outputSummary[idy2,colOutTsoil1+1]    = (2.8*soiltavg[idy,2] + 3.0*soiltavg[idy,3]) / casasoilthickness[2]
        outputSummary[idy2,colOutVSWC1+1]     = (2.8*vswc[idy,2]     + 3.0*vswc[idy,3])     / casasoilthickness[2]
        outputSummary[idy2,colOutFrznVSWC1+1] = 0.0 

        outputSummary[idy2,colOutTsoil1+2]    = (2.0*soiltavg[idy,3] + 10.0*soiltavg[idy,4] + 3.4*soiltavg[idy,5]) / casasoilthickness[3]
        outputSummary[idy2,colOutVSWC1+2]     = (2.0*vswc[idy,3]     + 10.0*vswc[idy,4]     + 3.4*vswc[idy,5])     / casasoilthickness[3]
        outputSummary[idy2,colOutFrznVSWC1+2] = 0.0 

        outputSummary[idy2,colOutTsoil1+3]    = (6.6*soiltavg[idy,5] + 15.0*soiltavg[idy,6] + 15.0*soiltavg[idy,7] + 4.3*soiltavg[idy,8]) / casasoilthickness[4]
        outputSummary[idy2,colOutVSWC1+3]     = (6.6*vswc[idy,5]     + 15.0*vswc[idy,6]     + 15.0*vswc[idy,7]     + 4.3*vswc[idy,8])     / casasoilthickness[4]
        outputSummary[idy2,colOutFrznVSWC1+3] = 0.0 

        outputSummary[idy2,colOutTsoil1+4]    = (10.7*soiltavg[idy,8] + 15.0*soiltavg[idy,9] + 15.0*soiltavg[idy,10] 
                                                 + 15.0*soiltavg[idy,11] + 30.0*soiltavg[idy,12] + 22.8*soiltavg[idy,13]) / casasoilthickness[5]
        outputSummary[idy2,colOutVSWC1+4]     = (10.7*vswc[idy,8] + 15.0*vswc[idy,9] + 15.0*vswc[idy,10]    
                                                 + 15.0*vswc[idy,11] + 30.0*vswc[idy,12] + 22.8*vswc[idy,13]) / casasoilthickness[5]
        outputSummary[idy2,colOutFrznVSWC1+4] = 0.0 

        outputSummary[idy2,colOutTsoil1+5]    = (7.2*soiltavg[idy,13] + 30.0*soiltavg[idy,14] + 250*soiltavg[idy,15]) / casasoilthickness[6]
        outputSummary[idy2,colOutVSWC1+5]     = (7.2*vswc[idy,13]     + 30.0*vswc[idy,14]     + 250*vswc[idy,15])     / casasoilthickness[6]
        outputSummary[idy2,colOutFrznVSWC1+5] = 0.0 
    }

}

#-------------------- Write results to the output file --------------------
write.csv(outputSummary, file=outputFileName, row.names=FALSE)
