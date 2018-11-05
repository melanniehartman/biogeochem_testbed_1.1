Running netcdf tools for a point.

Site: Niwot Ridge
Actual Lat/Lon: +40.06N, -105.62W (-105.62 + 360 = 254.38)
CLM grid cell Lat/Lon in clmGrid_IGBP.csv: +40.7/255, cellid = 09862


1. Set WRTCELL=1 for the above cell in clmGrid_IGBP.csv, WRTCELL=0 for all other cells.
2. I set files.ini as shown to create a point file with daily values from 1900-2010:

    h1         ! file type (h0=monthly, h1=daily)
    19010101   ! start (yyyymmdd)
    19201231   ! end (yyyymmdd)
    /project/tss/wwieder/CASACLM/clm_forcing/CRU_hist/clm4_5_12_r191_CLM45spHIST_CRU.clm2.h1.1850-01-01-00000.nc
    /project/tss/wwieder/CASACLM/surfdata_1.9x2.5_simyr1850_c130421_ADDwat4.nc
    /project/tss/bgc01/melannie/fndep_clm_hist_simyr1849-2006_1.9x2.5_c100428.nc
    ./CO2_1768-2010.txt

3. Run netcdfTools:
    module load tool/netcdf/4.3.2/gcc
    ./netcdfTools

4. The netcdfTools program creates the following files:

met_pt09862_1901_1920.nc
gridinfo_soil_pt09862.csv
gridinfo_igbpz_pt09862.csv - ilat and ilon used correctly? 

5. Copy the 3 files above to the run directory /project/tss/bgc01/melannie/CASACLM/POINT/EXAMPLE_PT_NIWOT/

In the following files, update the first ?? columns.

Determine npt (or pt, the position in the list) and ijgcm VALUES THAT CORRESPOND TO CELLID=09862: 
    npt=3066, ijgcm=9862 (you can get this info from gridinfo_igbpz.csv or corpse restart file)

casapool_init_pt09862.csv (uses pt only)
mimicspool_init_pt09862.csv (uses pt only)
corpsepool_init_pt09862.csv (uses both pt and cellid)


----------------------------------------------------------------------------------------------------
TO DO: 
Update netCDFTools so it does not write a negative value to iso in this file:
 Warning, iso has a negative value.
 Check file ./gridinfo_igbpz_pt09862.csv

Warning message from casaclm_mimics_corpse:

 Data alignment problem in ReadMetNcFile:
 cellid =         9862  cellCnt =            1
