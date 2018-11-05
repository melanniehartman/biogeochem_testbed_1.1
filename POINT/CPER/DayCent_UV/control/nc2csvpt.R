####################################################################################################
#
#  nc2csvpt.R
#
#  Author:
#    Melannie Hartman
#    melannie@ucar.edu
#    October 2, 2017
#
#  Description:
#    Read point met.nc from the CASA testbed and write values to a .csv file
#
#  NetCDF Input file format:
#  
#  netcdf met_pt09862_1901_1920 {
#  dimensions:
#       lon = 1 ;
#       lat = 1 ;
#       time = 7300 ;
#       nsoilyrs = 6 ;
#       myear = 20 ;
#  variables:
#       float lon(lon) ;
#           lon:long_name = "coordinate longitude" ;
#           lon:units = "degrees_east" ;
#           lon:_FillValue = 1.e+36f ;
#           lon:missing_value = 1.e+36f ;
#       float lat(lat) ;
#           lat:long_name = "coordinate latitude" ;
#           lat:units = "degrees_north" ;
#           lat:_FillValue = 1.e+36f ;
#           lat:missing_value = 1.e+36f ;
#       int year(myear) ;
#           year:long_name = "calendar years" ;
#       float landfrac(lat, lon) ;
#           landfrac:long_name = "land fraction from pft dataset" ;
#           landfrac:units = "unitless" ;
#           landfrac:_FillValue = 1.e+36f ;
#           landfrac:missing_value = 1.e+36f ;
#       int cellMissing(lat, lon) ;
#           cellMissing:long_name = "Missing Data Mask" ;
#           cellMissing:units = "0=no missing data, 1=missing data" ;
#       int cellid(lat, lon) ;
#           cellid:long_name = "Grid Cell ID" ;
#           cellid:units = "1..nlat*nlon" ;
#       float xtairk(time, lat, lon) ;
#           xtairk:long_name = "average daily air temperature" ;
#           xtairk:units = "K" ;
#           xtairk:_FillValue = 1.e+36f ;
#           xtairk:missing_value = 1.e+36f ;
#       float ndep(time, lat, lon) ;
#           ndep:long_name = "daily N deposition derived from annual N deposition" ;
#           ndep:units = "gN/m2/day" ;
#           ndep:_FillValue = 1.e+36f ;
#           ndep:missing_value = 1.e+36f ;
#       float xlai(time, lat, lon) ;
#           xlai:long_name = "Leaf Area Index" ;
#           xlai:units = "m2/m2" ;
#           xlai:_FillValue = 1.e+36f ;
#           xlai:missing_value = 1.e+36f ;
#       float xcnpp(time, lat, lon) ;
#           xcnpp:long_name = "net primary production" ;
#           xcnpp:units = "gC m-2 day-1" ;
#           xcnpp:_FillValue = 1.e+36f ;
#           xcnpp:missing_value = 1.e+36f ;
#       float xcgpp(time, lat, lon) ;
#           xcgpp:long_name = "gross primary production" ;
#           xcgpp:units = "gC m-2 day-1" ;
#           xcgpp:_FillValue = 1.e+36f ;
#           xcgpp:missing_value = 1.e+36f ;
#       float xtsoil(time, nsoilyrs, lat, lon) ;
#           xtsoil:long_name = "average daily soil temperature by layer" ;
#           xtsoil:units = "K" ;
#           xtsoil:_FillValue = 1.e+36f ;
#           xtsoil:missing_value = 1.e+36f ;
#       float xmoist(time, nsoilyrs, lat, lon) ;
#           xmoist:long_name = "volumetric soil liquid water content by layer" ;
#           xmoist:units = "m3/m3" ;
#           xmoist:_FillValue = 1.e+36f ;
#           xmoist:missing_value = 1.e+36f ;
#       float xfrznmoist(time, nsoilyrs, lat, lon) ;
#           xfrznmoist:long_name = "volumetric soil frozen water content by layer" ;
#           xfrznmoist:units = "m3/m3" ;
#           xfrznmoist:_FillValue = 1.e+36f ;
#           xfrznmoist:missing_value = 1.e+36f ;
#       
#  Output file format (met.csv):
#   #   Variable                  Description
#  --   ------------------        ----------------------------------------------------------------
#   1   calendar_year             Calendar year (4 digits)
#   2   doy                       Day of year (1-366)
#   3   Tair (degC)               Mean daily air temperature (deg C)
#   4   gpp (gC/m2/dy)            Gross Primary Production (gC/m2/day)
#   5   tsoil_1 (deg C)           Mean daily soil temperature 0-2.2 cm (deg C)
#   6   tsoil_2 (deg C)           Mean daily soil temperature 2.2-8.0 cm (deg C)
#   7   tsoil_3 (deg C)           Mean daily soil temperature 8.0-23.4 cm (deg C)
#   8   tsoil_4 (deg C)           Mean daily soil temperature 23.4-64.3 cm (deg C)
#   9   tsoil_5 (deg C)           Mean daily soil temperature 64.3 to 172.8 cm (deg C)
#  10   tsoil_6 (deg C)           Mean daily soil temperature172.8 to 460 cm (deg C)
#  11   vswc_liq_1 (m3/m3)        Volumetric soil liquid water content 0-2.2 cm (0.0-1.0)
#  12   vswc_liq_2 (m3/m3)        Volumetric soil liquid water content 2.2-8.0 cm (0.0-1.0)
#  13   vswc_liq_3 (m3/m3)        Volumetric soil liquid water content 8.0-23.4 cm (0.0-1.0)
#  14   vswc_liq_4 (m3/m3)        Volumetric soil liquid water content 23.4-64.3 cm (0.0-1.0)
#  15   vswc_liq_5 (m3/m3)        Volumetric soil liquid water content 64.3 to 172.8 cm (0.0-1.0)
#  16   vswc_liq_6 (m3/m3)        Volumetric soil liquid water content 172.8 to 460 cm (0.0-1.0)
#  17   vswc_frzn_1 (m3/m3)       Volumetric soil frozen water content 0-2.2 cm (0.0-1.0)
#  18   vswc_frzn_2 (m3/m3)       Volumetric soil frozen water content 2.2-8.0 cm (0.0-1.0)
#  19   vswc_frzn_3 (m3/m3)       Volumetric soil frozen water content 8.0-23.4 cm (0.0-1.0)
#  20   vswc_frzn_4 (m3/m3)       Volumetric soil frozen water content 23.4-64.3 cm (0.0-1.0)
#  21   vswc_frzn_5 (m3/m3)       Volumetric soil frozen water content 64.3 to 172.8 cm (0.0-1.0)
#  22   vswc_frzn_6 (m3/m3)       Volumetric soil frozen water content 172.8 to 460 cm (0.0-1.0)
#  23   Nep                       Atmospheric N deposition (gN/m2/day)
#
####################################################################################################

rm(list=ls())    # Clear memory

Sys.getenv("R_LIBS_USER")
#library(ncdf.tools)
#library(sp)
#library(ncdf4)
library(RNetCDF)

#ls("package:ncdf.tools")
#ls("package:sp")
#ls("package:ncdf4")
ls("package:RNetCDF")


workdir <<- "E:\\dev\\NCAR\\CASACLM\\POINT\\EXAMPLE_PT_NIWOT_FOREST\\"
setwd(workdir)

#inputNcFileName = paste(workdir, "met_pt09862_1901_1920.nc", sep="")
inputNcFileName = paste(workdir, "met_pt09862_1901_2010.nc", sep="")

outputFileName = paste(workdir, "met_niwot_pt09862_1901_2010.csv", sep="")
OutHdr <- c("calendar_year","doy","Tair(degC)","gpp(gC/m2/dy)",
             "Tsoil_1(deg C)","Tsoil_2(deg C)","Tsoil_3(deg C)","Tsoil_4(deg C)","Tsoil_5(deg C)","Tsoil_6(deg C)",
             "vswc_liq_1(m3/m3)","vswc_liq_2(m3/m3)","vswc_liq_3(m3/m3)","vswc_liq_4(m3/m3)","vswc_liq_5(m3/m3)","vswc_liq_6(m3/m3)",
             "vswc_frzn_1(m3/m3)","vswc_frzn_2(m3/m3)","vswc_frzn_3(m3/m3)","vswc_frzn_4(m3/m3)","vswc_frzn_5(m3/m3)","vswc_frzn_6(m3/m3)",
             "Ndep(gN/m2/dy)")

# Important columns in output file
colOutYr = 1
colOutDOY = 2
colOutTair = 3
colOutGPP = 4
colOutTsoil1 = 5
colOutVSWC1 = 11
colOutFrznVSWC1 = 17
colOutNdep = 23

# Open input NetCDF file
ncin = open.nc(inputNcFileName)
print(ncin)

# Get lon and lat variables, which are the dimensions of depth. For this specific dataset they have the names lon and lat
lon=var.get.nc(ncin,"lon")
lat=var.get.nc(ncin,"lat")
year=var.get.nc(ncin,"year")
cellid=var.get.nc(ncin,"cellid")

nlon = length(lon)
nlat = length(lon)
ntime = dim.inq.nc(ncin,"time")$length
nLyrs = dim.inq.nc(ncin,"nsoilyrs")$length
myear = dim.inq.nc(ncin,"myear")$length

xtairk=var.get.nc(ncin,"xtairk")
ndep=var.get.nc(ncin,"ndep")
xcgpp=var.get.nc(ncin,"xcgpp")
xtsoil=var.get.nc(ncin,"xtsoil")
xfrznmoist=var.get.nc(ncin,"xfrznmoist")
xmoist=var.get.nc(ncin,"xmoist")

dim(xtsoil)   # Shows 6, 7300 (nsoilyrs, time) for xtsoil(time, nsoilyrs, lat, lon)
              # when the met.nc was 20 years long (20*365 days). 
              # It appears that lat/lon dimensions for a single point were ignored

maxDays <- ntime
maxCols <- length(OutHdr)
outputSummary <- array(data = 0.0, dim = c(maxDays, maxCols))
colnames(outputSummary) = OutHdr 

# Copy netcdf input files into .csv outputSummay table structure
dayCnt=0
for (iyr in year)
{
    for (idy in 1:365)
    {
        dayCnt = dayCnt + 1
        outputSummary[dayCnt,colOutYr] = iyr
        outputSummary[dayCnt,colOutDOY] = idy
        outputSummary[dayCnt,colOutTair] = xtairk[dayCnt] - 273.15
        outputSummary[dayCnt,colOutGPP] = xcgpp[dayCnt] 
        outputSummary[dayCnt,colOutNdep] = ndep[dayCnt]

        #outputSummary[dayCnt,colOutTsoil1:colOutTsoil1+nLyrs-1] = xtsoil[dayCnt,1:nLyrs] - 273.15
        #outputSummary[dayCnt,colOutVSWC1:colOutVSWC1+nLyrs-1] = xmoist[dayCnt,1:nLyrs] 
        #outputSummary[dayCnt,colOutFrznVSWC1:colOutFrznVSWC1+nLyrs-1] = xfrznmoist[dayCnt,1:nLyrs] 
        for (ilyr in 1:nLyrs)
        {
            outputSummary[dayCnt,colOutTsoil1+ilyr-1] = xtsoil[ilyr,dayCnt] - 273.15
            outputSummary[dayCnt,colOutVSWC1+ilyr-1] = xmoist[ilyr,dayCnt] 
            outputSummary[dayCnt,colOutFrznVSWC1+ilyr-1] = xfrznmoist[ilyr,dayCnt] 
        }

    }
}


#-------------------- Write results to the output file --------------------
write.csv(outputSummary, file=outputFileName, row.names=FALSE)