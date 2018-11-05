program main

   ! Read in climate data from CLM h1 (daily) NetCDF files 
   !
   ! Melannie Hartman
   !   Last update:
   !     October 7, 2013
   !     September 12, 2016
   !     September 24, 2018 - Updated for CLM 5 (previously 4 and 4.5)
   !
   ! h1 daily files - daily values for one year (365 days)
   !   Example files:
   !     /project/bgc01/bonan/casa-cnp/clm/clm45sp_2deg4506_hist.clm2.h1.1901-01-01-00000.nc
   !         :
   !     /project/bgc01/bonan/casa-cnp/clm/clm45sp_2deg4506_hist.clm2.h1.2010-01-01-00000.nc
   !     
   !     OR
   !
   !     /project/bgc01/bonan/casa-cnp/clm/clm45sp_2deg4506_rcp85.clm2.h1.2006-01-01-00000.nc
   !         :
   !     /project/bgc01/bonan/casa-cnp/clm/clm45sp_2deg4506_rcp85.clm2.h1.2100-01-01-00000.nc
   !
  
   ! USES
   use clm_netcdf_tools
   implicit none

   ! LOCAL VARIABLES
   character*100    :: filesin           ! files.ini

!  Some utilities that are available:
!  call printNCfileInfo('/project/tss/slevis/forMelannie/monthly/clm45sp_2deg4506_hist.clm2.h0.2000-01.nc')

   filesin = 'files.ini'

   !Read and process monthly, daily, or hourly climate files one year at a time
   !This function also runs the aggregated canopy model each year when hourly climate is read.

   call readClimateSequence(filesin)

   write(*,*) 'Simulation Complete.'

end program main

