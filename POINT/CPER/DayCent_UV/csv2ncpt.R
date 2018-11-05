####################################################################################################
#
#  csv2ncpt.R
#
#  Author:
#    Melannie Hartman
#    melannie@ucar.edu
#    October 9, 2017
#    January 23, 2018
#    May 7, 2018
#
#  Description:
#    Read met.csv (23 columns) and write contents to NetCDF file in met.nc format for the CASA testbed
#       
#  Input file format (met.csv):
#   #   Variable                  Description
#  --   ------------------        ----------------------------------------------------------------
#   1   calendar_year             Calendar year (4 digits)
#   2   doy                       Day of year (1-366)
#   3   Tmax (degC)               Maximum daily air temperature (deg C)
#   4   Tmin (degC)               Minimum daily air temperature (deg C)
#   5   precip (cm/dy)            Daily precipitation (cm)
#   6   gpp (gC/m2/dy)            Gross Primary Production (gC/m2/day)
#   7   tsoil_1 (deg C)           Mean daily soil temperature 0-2.2 cm (deg C)
#   8   tsoil_2 (deg C)           Mean daily soil temperature 2.2-8.0 cm (deg C)
#   9   tsoil_3 (deg C)           Mean daily soil temperature 8.0-23.4 cm (deg C)
#  10   tsoil_4 (deg C)           Mean daily soil temperature 23.4-64.3 cm (deg C)
#  11   tsoil_5 (deg C)           Mean daily soil temperature 64.3 to 172.8 cm (deg C)
#  12   tsoil_6 (deg C)           Mean daily soil temperature172.8 to 460 cm (deg C)
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
#  25   Ndep (gN/m2/day)          Atmospheric N deposition (gN/m2/day)
#
#
#  NetCDF Output file format:
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
####################################################################################################

rm(list=ls())    # Clear memory

Sys.getenv("R_LIBS_USER")
library(RNetCDF)
ls("package:RNetCDF")

workdir <<- "E:\\dev\\NCAR\\LTER\\CPER\\DayCent_UV\\"
setwd(workdir)

# Important columns in input file
colInYr = 1
colInDOY = 2
colInTmax = 3
colInTmin = 4
colInGPP = 6
colInTsoil1 = 7
colInVSWC1 = 13
colInFrznVSWC1 = 19
colInNdep = 25


# Assign input and output file names
#inputCSVFileName = paste(workdir, "met_niwot_pt09862.csv", sep="")
#inputCSVFileName = paste(workdir, "met_CPER_1912_2016_v2.csv", sep="")
#outputNcFileName = paste(workdir, "met_CPER_1912_2016_v2.nc", sep="")
inputCSVFileName = paste(workdir, "met_CPER_1912_1951_v2.csv", sep="")
outputNcFileName = paste(workdir, "met_CPER_1912_1951_v2.nc", sep="")

#Read .csv file
outputSummary <- read.table(inputCSVFileName, header = TRUE, sep = ",", dec = ".")

nPts = 1

# Values of Nc file dimensions
nlon = nPts         # lon
nlat = nPts         # lat
    # myear

maxDays = length(outputSummary[,1])
maxCols <- length(outputSummary[1,])
maxLyrs = 6
maxYrs = as.integer(maxDays/365)
if (maxDays/365 - maxYrs != 0)
{
    #Exit if maxDays is not a multiple of 365
    exit
}

fMISSING_VAL = 1.0e+36  # Fill_Value or missing_value

# Because dimensions in R are in Column Major Order (the first 
# array index varies the most rapidly) dimensions are in the opposite 
# order that they appear in the NetCDF file with ncdump.

year <- array(data = 0, dim = c(maxYrs))
xtairk <- array(data = 0.0, dim = c(nlon,nlat,maxDays))
xlai <- array(data = 0.0, dim = c(nlon,nlat,maxDays))
xcgpp <- array(data = 0.0, dim = c(nlon,nlat,maxDays))
xcnpp <- array(data = 0.0, dim = c(nlon,nlat,maxDays))
ndep <- array(data = 0.0, dim = c(nlon,nlat,maxDays))
xtsoil <- array(data = 0.0, dim = c(nlon,nlat,maxLyrs,maxDays))
xmoist <- array(data = 0.0, dim = c(nlon,nlat,maxLyrs,maxDays))
xfrznmoist <- array(data = 0.0, dim = c(nlon,nlat,maxLyrs,maxDays))


# Write values from the .csv file into arrays that will be written to outputNcFileName

dayCnt=0
for (iyr in 1:maxYrs)
{
    for (idy in 1:365)
    {
        dayCnt = dayCnt + 1
        calyr = outputSummary[dayCnt,colInYr]
        year[iyr] = calyr
        doy = outputSummary[dayCnt,colInDOY] 
        xtairk[dayCnt] = 0.5*(outputSummary[dayCnt,colInTmin] + outputSummary[dayCnt,colInTmax]) + 273.15
        xlai[1,1,dayCnt] = fMISSING_VAL                           # LAI is not used by the testbed, placeholder only.
        xcgpp[1,1,dayCnt] = outputSummary[dayCnt,colInGPP] 
        xcnpp[1,1,dayCnt] = outputSummary[dayCnt,colInGPP] / 2.0  # Estimate of NPP - placeholder only.
        ndep[1,1,dayCnt] = outputSummary[dayCnt,colInNdep] 

        for (ilyr in 1:maxLyrs)
        {
            xtsoil[1,1,ilyr,dayCnt] = outputSummary[dayCnt,colInTsoil1+ilyr-1] + 273.15 
            xmoist[1,1,ilyr,dayCnt] = outputSummary[dayCnt,colInVSWC1+ilyr-1]
            xfrznmoist[1,1,ilyr,dayCnt] = outputSummary[dayCnt,colInFrznVSWC1+ilyr-1] 
        }

    }
}


#-------------------- Write results to the output file --------------------
ntime = maxDays     # size of time dimension
nsoilyrs = maxLyrs  # size of nsoilyrs dimension
myear = maxYrs      # size of myear dimension

# Arrays for holding dimension ids for multi-dimensional variables
dims1 = array(data = 0, dim = c(1))
dims2 = array(data = 0, dim = c(2))
dims3 = array(data = 0, dim = c(3))
dims4 = array(data = 0, dim = c(4))

lat = array(data = 0.0, dim = c(nlat))
lon = array(data = 0.0, dim = c(nlon))
cellid = array(data = 0, dim = c(nlon,nlat))
cellMissing = array(data = 0, dim = c(nlon,nlat))
landfrac = array(data = 0, dim = c(nlon,nlat))

lat[1] = 40.81
lon[1] = -104.74 + 360
cellid[1,1] = 1
cellMissing[1,1] = 0
landfrac[1,1] = 1.0

# Open input NetCDF file
ncout = create.nc(outputNcFileName)

# Define outputNcFileName dimensions
#-->dim.def.nc(ncfile, dimname, dimlength=1, unlim=FALSE)

  dim.def.nc(ncout, "lon", nlon, unlim=FALSE)

  dim.def.nc(ncout, "lat", nlat, unlim=FALSE)

  dim.def.nc(ncout, "time", ntime, unlim=FALSE)

  dim.def.nc(ncout, "nsoilyrs", nsoilyrs, unlim=FALSE)

  dim.def.nc(ncout, "myear", myear, unlim=FALSE)


# Define outputNcFileName variables
#-->var.def.nc(ncfile, varname, vartype, dimensions)

  dims1[1] = dim.inq.nc(ncout,"lat")$id
  var.def.nc(ncout, "lon", "NC_FLOAT", dims1)

  dims1[1] = dim.inq.nc(ncout,"lon")$id
  var.def.nc(ncout, "lat", "NC_FLOAT", dims1)

  dims1[1] = dim.inq.nc(ncout,"myear")$id
  var.def.nc(ncout, "year", "NC_INT", dims1)

  # Because dimensions in R are in Column Major Order (the first 
  # array index varies the most rapidly) dimensions are in the opposite 
  # order that they appear in the NetCDF file with ncdump. 

  dims2[1] = dim.inq.nc(ncout,"lon")$id
  dims2[2] = dim.inq.nc(ncout,"lat")$id

  var.def.nc(ncout, "landfrac", "NC_FLOAT", dims2)

  var.def.nc(ncout, "cellMissing", "NC_INT", dims2)

  var.def.nc(ncout, "cellid", "NC_INT", dims2)


  # Because dimensions in R are in Column Major Order (the first 
  # array index varies the most rapidly) dimensions are in the opposite 
  # order that they appear in the NetCDF file with ncdump. 

  dims3[1] = dim.inq.nc(ncout,"lon")$id
  dims3[2] = dim.inq.nc(ncout,"lat")$id
  dims3[3] = dim.inq.nc(ncout,"time")$id

  var.def.nc(ncout, "xtairk", "NC_FLOAT", dims3)

  var.def.nc(ncout, "ndep", "NC_FLOAT", dims3)

  var.def.nc(ncout, "xlai", "NC_FLOAT", dims3)

  var.def.nc(ncout, "xcnpp", "NC_FLOAT", dims3)

  var.def.nc(ncout, "xcgpp", "NC_FLOAT", dims3)


  # Because dimensions in R are in Column Major Order (the first 
  # array index varies the most rapidly) dimensions are in the opposite 
  # order that they appear in the NetCDF file with ncdump. 

  dims4[1] = dim.inq.nc(ncout,"lon")$id
  dims4[2] = dim.inq.nc(ncout,"lat")$id
  dims4[3] = dim.inq.nc(ncout,"nsoilyrs")$id
  dims4[4] = dim.inq.nc(ncout,"time")$id
 
  var.def.nc(ncout, "xtsoil", "NC_FLOAT", dims4)
 
  var.def.nc(ncout, "xmoist", "NC_FLOAT", dims4)
 
  var.def.nc(ncout, "xfrznmoist", "NC_FLOAT", dims4)


# Global attributes
  attr_name = "Forcings for the CASACNP testbed"
  att.put.nc(ncout, "NC_GLOBAL", "title", "NC_CHAR", attr_name)
 
  attr_name = "NOTE: None of the variables are weighted by land fraction!"
  att.put.nc(ncout, "NC_GLOBAL", "comment", "NC_CHAR", attr_name)
 
  attr_name = "Created on"
  attr_name = paste(attr_name, date(), sep=" ")
  att.put.nc(ncout, "NC_GLOBAL", "history", "NC_CHAR", attr_name)
 
  #attr_name = "CLM Model"
  #att.put.nc(ncout, "NC_GLOBAL", "source", "NC_CHAR", attr_name)
 

# Attributes of the variables
#-->att.put.nc(ncfile, variable, name, type, value)

   # Attributes of year variable
   attr_name = "calendar years"
   att.put.nc(ncout, "year", "long_name", "NC_CHAR", attr_name)

   # Attributes of lon variable
   attr_name = "coordinate longitude"
   attr_units = "degrees_east"
   att.put.nc(ncout, "lon", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "lon", "units", "NC_CHAR", attr_units)
 
   # Attributes of lat variable
   attr_name = "coordinate latitude"
   attr_units = "degrees_north"
   att.put.nc(ncout, "lat", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "lat", "units", "NC_CHAR", attr_units)
 
   # Attributes of landfrac variable
   attr_name = "land fraction from pft dataset"
   attr_units = "unitless"
   att.put.nc(ncout, "landfrac", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "landfrac", "units", "NC_CHAR", attr_units)

   # Attributes of cellMissing variable
   attr_name = "Missing Data Mask"
   attr_units = "0=no missing data, 1=missing data"
   att.put.nc(ncout, "cellMissing", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "cellMissing", "units", "NC_CHAR", attr_units)

   # Attributes of cellid variable
   attr_name = "Grid Cell ID"
   attr_units = "1..nlat*nlon"
   att.put.nc(ncout, "cellid", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "cellid", "units", "NC_CHAR", attr_units)

   # Attributes of xcnpp variable
   attr_name = "net primary production"
   attr_units = "gC m-2 day-1"
   att.put.nc(ncout, "xcnpp", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "xcnpp", "units", "NC_CHAR", attr_units)
   att.put.nc(ncout, "xcnpp", "_FillValue", "NC_FLOAT", fMISSING_VAL)
   att.put.nc(ncout, "xcnpp", "missing_value", "NC_FLOAT", fMISSING_VAL)

 
   # Attributes of xcgpp variable
   attr_name = "gross primary production"
   attr_units = "gC m-2 day-1"
   att.put.nc(ncout, "xcgpp", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "xcgpp", "units", "NC_CHAR", attr_units)
   att.put.nc(ncout, "xcgpp", "_FillValue", "NC_FLOAT", fMISSING_VAL)
   att.put.nc(ncout, "xcgpp", "missing_value", "NC_FLOAT", fMISSING_VAL)

   # Attributes of xlai variable
   attr_name = "Leaf Area Index"
   attr_units = "m2/m2"
   att.put.nc(ncout, "xlai", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "xlai", "units", "NC_CHAR", attr_units)
   att.put.nc(ncout, "xlai", "_FillValue", "NC_FLOAT", fMISSING_VAL)
   att.put.nc(ncout, "xlai", "missing_value", "NC_FLOAT", fMISSING_VAL)
 
   # Attributes of xtairk variable
   attr_name = "average daily air temperature"
   attr_units = "K"
   att.put.nc(ncout, "xtairk", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "xtairk", "units", "NC_CHAR", attr_units)
   att.put.nc(ncout, "xtairk", "_FillValue", "NC_FLOAT", fMISSING_VAL)
   att.put.nc(ncout, "xtairk", "missing_value", "NC_FLOAT", fMISSING_VAL)

   # Attributes of ndepDay variable
   attr_name = "daily N deposition derived from annual N deposition"
   attr_units = "gN/m2/day"
   att.put.nc(ncout, "ndep", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "ndep", "units", "NC_CHAR", attr_units)
   att.put.nc(ncout, "ndep", "_FillValue", "NC_FLOAT", fMISSING_VAL)
   att.put.nc(ncout, "ndep", "missing_value", "NC_FLOAT", fMISSING_VAL)

   # Attributes of xtsoil variable
   attr_name = "average daily soil temperature by layer"
   attr_units = "K"
   att.put.nc(ncout, "xtsoil", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "xtsoil", "units", "NC_CHAR", attr_units)
   att.put.nc(ncout, "xtsoil", "_FillValue", "NC_FLOAT", fMISSING_VAL)
   att.put.nc(ncout, "xtsoil", "missing_value", "NC_FLOAT", fMISSING_VAL)

   # Attributes of xmoist variable
   attr_name = "volumetric soil liquid water content by layer"
   attr_units = "m3/m3"
   att.put.nc(ncout, "xmoist", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "xmoist", "units", "NC_CHAR", attr_units)
   att.put.nc(ncout, "xmoist", "_FillValue", "NC_FLOAT", fMISSING_VAL)
   att.put.nc(ncout, "xmoist", "missing_value", "NC_FLOAT", fMISSING_VAL)

   # Attributes of xfrznmoist variable
   attr_name = "volumetric soil frozen water content by layer"
   attr_units = "m3/m3"
   att.put.nc(ncout, "xfrznmoist", "long_name", "NC_CHAR", attr_name)
   att.put.nc(ncout, "xfrznmoist", "units", "NC_CHAR", attr_units)
   att.put.nc(ncout, "xfrznmoist", "_FillValue", "NC_FLOAT", fMISSING_VAL)
   att.put.nc(ncout, "xfrznmoist", "missing_value", "NC_FLOAT", fMISSING_VAL)


# End Define Mode

# Write variable values to outputNcFileName
#-->var.put.nc(ncfile, variable, data, start=NA, count=NA, na.mode=0, pack=FALSE)

   var.put.nc(ncout, "lon", lon)

   var.put.nc(ncout, "lat", lat)

   var.put.nc(ncout, "year", year)

   var.put.nc(ncout, "landfrac", landfrac)

   var.put.nc(ncout, "cellMissing", cellMissing)

   var.put.nc(ncout, "cellid", cellid)

   var.put.nc(ncout, "xtairk", xtairk)

   var.put.nc(ncout, "ndep", ndep)

   var.put.nc(ncout, "xlai", xlai)

   var.put.nc(ncout, "xcnpp", xcnpp)

   var.put.nc(ncout, "xcgpp", xcgpp)

   var.put.nc(ncout, "xtsoil", xtsoil)

   var.put.nc(ncout, "xmoist", xmoist)

   var.put.nc(ncout, "xfrznmoist", xfrznmoist)


# Close outputNcFileName
close.nc(ncout)
