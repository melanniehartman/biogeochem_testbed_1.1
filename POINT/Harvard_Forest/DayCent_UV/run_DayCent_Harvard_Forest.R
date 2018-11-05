###############################################################################
#
# run_DayCent_Harvard_Forest.R
#
# Author: Melannie Hartman 
#         June 25, 2018
#         July 23, 2018
#         August 20, 2018
#
# Description:
#   This R script runs DayCent model simulations for Harvard Forest.
#   There are two treatments: control and N addition.
#
###############################################################################

# Reinitialize the variables in memory before rerunning the script. 
rm(list=ls())

# Path to files
modelPath = "E:/dev/NCAR/CASACLM/POINT/Harvard_Forest/DayCent_UV"
setwd(modelPath)

# Harvard Forest control run (no disturbance, with N limitation): (0-2006)
unlink("harvardforest_control.bin")
unlink("harvardforest_control.lis")
system("DayCent_UV.exe -s harvardforest_control -n harvardforest_control")
system("DayCent_list100_UV.exe harvardforest_control harvardforest_control outvars.txt")
file.rename("vswc.out", "vswc_harvardforest_control.out")
file.rename("psyn.out", "psyn_harvardforest_control.out")
file.rename("soiltavg.out", "soiltavg_harvardforest_control.out")
file.rename("dc_sip.csv", "dc_sip_harvardforest_control.csv")
file.rename("daily.out", "daily_harvardforest_control.out")

# Harvard Forest with N addition to mimic N-unlimited C-only run: (0-2006)
unlink("harvardforest_Naddition.bin")
unlink("harvardforest_Naddition.lis")
system("DayCent_UV.exe -s harvardforest_Naddition -n harvardforest_Naddition")
system("DayCent_list100_UV.exe harvardforest_Naddition harvardforest_Naddition outvars.txt")
file.rename("vswc.out", "vswc_harvardforest_Naddition.out")
file.rename("psyn.out", "psyn_harvardforest_Naddition.out")
file.rename("soiltavg.out", "soiltavg_harvardforest_Naddition.out")
file.rename("dc_sip.csv", "dc_sip_harvardforest_Naddition.csv")
file.rename("daily.out", "daily_harvardforest_Naddition.out")

system("perl lis2csv.pl")
system("perl out2csv.pl")
