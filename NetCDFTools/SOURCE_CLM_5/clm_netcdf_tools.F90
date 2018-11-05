module clm_netcdf_tools

!-----------------------------------------------------------------------
!BOP
!
!  !MODULE: clm_netcdf_tools
!
!  !DESCRIPTION:
!
!  !USES:
!   use clm_common
!   use canopy_model
    use casacnp_model
!
!  !PUBLIC TYPES:
   implicit none

   !Parameters are defined in clm_common

! !PUBLIC MEMBER FUNCTIONS:
   public :: readDailyHistoryFile         !! Added 8/11/2014
   public :: readClimateSequence
   public :: readInitFile
   public :: readNdepositionFile
   public :: readhisth0File               !! Replaces readSurfaceData. -mdh 9/24/2018
   public :: soilWaterPotential

! !PUBLIC DATA MEMBERS:
!
! !PRIVATE MEMBER FUNCTIONS:
!
! !PRIVATE DATA MEMBERS:
!
! !I/O text files
!  unit=10      Input (required)  : Initialization file (files.ini)
!  unit=14      Input (required)  : transient CO2 concentrations (year  CO2ppm)
!  unit=12      Output (optional) : daily climate text file for LATINDX, LONINDX (dailyClim_yyyy.csv)
!  unit=16      Output (optional) : daily canopy model gpp text file for LATINDX, LONINDX (gpp.csv)

! !REVISION HISTORY:
!  Created by Melannie Hartman
!  melannie@ucar.edu
!  July 2013-March 2014
!  Revisions
!  * Remove NPP and LAI from met.nc files -mdh 11/6/2017
!  * Updated for CLM 5.0 soil layer structure. -mdh 9/17/2018
!
!  NOTES:
!    
!  NetCDF float are real(4), double is real(8). If types are not correct SIGSEGV occurs:
!    Program received signal 11 (SIGSEGV): Segmentation fault.
!
!EOP
!-----------------------------------------------------------------------

contains

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: readhisth0File
!
! !INTERFACE:
   subroutine readhisth0File(nchisth0filename, nlon, nlat, nlevsoi, nlevgrnd, &
                              nbedrock, pctsand, pctclay, orgdens, watsat, watfc)

! !DESCRIPTION:
!  Read nbedrock, cellsand, cellclay, cellorg, watsat, and watfc from the first monthly history file.
!  Return those arrays and their dimensions. 
!  Note: Code to read LATIXY and LONGXY is available but commented out to save execution time.
!
! !USES:
!  none
!
! !ARGUMENTS:
   character(len=*), intent(in) :: nchisth0filename     ! netcdf filename for first monthly history file with surface data
   integer, intent(out) :: nlon                         ! length of longitude dimensions 
   integer, intent(out) :: nlat                         ! length of latitude dimension 
   integer, intent(out) :: nlevsoi                      ! number of soil levels (20)
   integer, intent(out) :: nlevgrnd                     ! number of soil + bedrock levels (25)

   integer, allocatable, intent(out) :: nbedrock(:,:)   ! nbedrock=index of shallowest bedrock layer
   real(4), allocatable, intent(out) :: pctsand(:,:,:)  ! pctsand(lon, lat, nlevgrnd) 
   real(4), allocatable, intent(out) :: pctclay(:,:,:)  ! pctclay(lon, lat, nlevgrnd) 
   real(4), allocatable, intent(out) :: orgdens(:,:,:)  ! orgdens(lon, lat, nlevgrnd) (kg/m3 (assumed C content 0.58 gC per gOM)) 
   real(4), allocatable, intent(out) :: watsat(:,:,:)   ! watsat(lon, lat, nlevgrnd) saturated water content (m3/m3)
   real(4), allocatable, intent(out) :: watfc(:,:,:)    ! watfc(lon, lat, nlevgrnd) water field capacity (m3/m3)

! !CALLED FROM:
!  subroutine readClimateSequence
!
! !REVISION HISTORY:
!  Created by Melannie Hartman
!
!EOP

! !LOCAL VARIABLES:
      integer :: i,j,k
      integer :: ncid                           ! netcdf file ID
      integer :: status                         ! function return status
      integer :: ntime                          ! number of times (should be 1!)
      integer :: nlat_dimid                     ! netcdf dimension id
      integer :: nlon_dimid                     ! netcdf dimension id
      integer :: nlevsoi_dimid                  ! netcdf dimension id
      integer :: nlevgrnd_dimid                 ! netcdf dimension id
      integer :: pctsand_varid                  ! netcdf variable id
      integer :: pctclay_varid                  ! netcdf variable id
      integer :: orgdens_varid                  ! netcdf variable id
      integer :: watsat_varid                   ! netcdf variable id
      integer :: watfc_varid                    ! netcdf variable id
      integer :: nbedrock_varid                 ! netcdf variable id
      integer :: start2(2), count2(2)           ! start and count arrays for reading 2-D data from netcdf files
      !integer :: start3(3), count3(3)          ! start and count arrays for reading 3-D data from netcdf files
      integer :: start4(4), count4(4)           ! start and count arrays for reading 4-D data from netcdf files

      real(4), allocatable :: cellsand(:,:,:,:)    ! percent sand: cellsand(time,nlevgrnd,lat,lon) 
      real(4), allocatable :: cellclay(:,:,:,:)    ! percent clay: cellclay(time,nlevgrnd,lat,lon) 
      real(4), allocatable :: cellorg(:,:,:,:)     ! organic matter density: cellorg(time,nlevgrnd,lat,lon) (kg/m3 (assumed C content 0.58 gC per gOM)) 
      real(4), allocatable :: cellwatsat(:,:,:,:)  ! saturated water content:  watsat(time,nlevgrnd,lat,lon) (m3/m3)
      real(4), allocatable :: cellwatfc(:,:,:,:)   ! water field capacity: watfc(time,nlevgrnd,lat,lon) (m3/m3)

!-----------------------------------------------------------------------
      if (verbose .ge. 0) print *, "Reading surface data from file ", trim(nchisth0filename), "..."

      ! Open netcdf file
      status = nf_open(nchisth0filename, nf_nowrite, ncid)
      if (status /= nf_noerr) call handle_err(status, "nf_open")
     
      ! Get dimension ids
      ! There may be more dimensions in the file - if so use the examples below to read them in
         
      status = nf_inq_dimid(ncid, "lat", nlat_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(lat)")

      status = nf_inq_dimid(ncid, "lon", nlon_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(lon)")

      status = nf_inq_dimid(ncid, "levsoi", nlevsoi_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(levsoi)")

      status = nf_inq_dimid(ncid, "levgrnd", nlevgrnd_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(levgrnd)")

      ! Get dimension sizes

      status = nf_inq_dimlen(ncid, nlat_dimid, nlat)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(lat)")

      status = nf_inq_dimlen(ncid, nlon_dimid, nlon)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(lon)")

      status = nf_inq_dimlen(ncid, nlevsoi_dimid, nlevsoi)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(levsoi)")

      status = nf_inq_dimlen(ncid, nlevgrnd_dimid, nlevgrnd)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(levgrnd)")

      if (verbose .ge. 1) then
         print *, "  readhisth0File: nlat: ", nlat
         print *, "  readhisth0File: nlon: ", nlon
         print *, "  readhisth0File: nlevsoi: ", nlevsoi
         print *, "  readhisth0File: nlevgrnd: ", nlevgrnd
      endif


      ! Get variable ids

      status = nf_inq_varid(ncid, "cellsand", pctsand_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(cellsand)")

      status = nf_inq_varid(ncid, "cellclay", pctclay_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(cellclay)")

      status = nf_inq_varid(ncid, "cellorg", orgdens_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(cellorg)")

      status = nf_inq_varid(ncid, "watsat", watsat_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(watsat)")

      status = nf_inq_varid(ncid, "watfc", watfc_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(watfc)")

      status = nf_inq_varid(ncid, "nbedrock", nbedrock_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(nbedrock)")

! Dimensions in FORTRAN are in Column Major Order: the first array index varies the most rapidly.
! Note, in NetCDF file the dimensions appear in the opposite order: lat, lon (2-D) 

      allocate(nbedrock(1:nlon, 1:nlat))

      nbedrock(:,:) = 0

      start2 = (/ 1, 1 /)
      count2 = (/ nlat, nlon /)

      status = nf_get_var(ncid, nbedrock_varid, nbedrock, start2, count2)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(nbedrock)")
      
! Dimensions in FORTRAN are in Column Major Order: the first array index varies the most rapidly.
! Note, in NetCDF file the dimensions appear in the opposite order: time, nlevgrnd, nlat, nlon (3-D) 
! start and count are in the same order as in netCDF files, whereas variable dimensions
! are in the opposite order.

      ntime = 1

      ! Variables read from netcdf file
      allocate(cellsand(1:nlon, 1:nlat, 1:nlevgrnd, 1:ntime))
      allocate(cellclay(1:nlon, 1:nlat, 1:nlevgrnd, 1:ntime))
      allocate(cellorg(1:nlon, 1:nlat, 1:nlevgrnd, 1:ntime))
      allocate(cellwatsat(1:nlon, 1:nlat, 1:nlevgrnd, 1:ntime))
      allocate(cellwatfc(1:nlon, 1:nlat, 1:nlevgrnd, 1:ntime))

      cellsand(:,:,:,:) = 0.0
      cellclay(:,:,:,:) = 0.0
      cellorg(:,:,:,:) = 0.0
      cellwatsat(:,:,:,:) = 0.0
      cellwatfc(:,:,:,:) = 0.0

      ! Variables returned by this subroutine
      allocate(pctsand(1:nlon, 1:nlat, 1:nlevgrnd))
      allocate(pctclay(1:nlon, 1:nlat, 1:nlevgrnd))
      allocate(orgdens(1:nlon, 1:nlat, 1:nlevgrnd))
      allocate(watsat(1:nlon, 1:nlat, 1:nlevgrnd))
      allocate(watfc(1:nlon, 1:nlat, 1:nlevgrnd))

      pctsand(:,:,:) = 0.0
      pctclay(:,:,:) = 0.0
      orgdens(:,:,:) = 0.0
      watsat(:,:,:) = 0.0
      watfc(:,:,:) = 0.0

      !start3 = (/ 1, 1, 1 /)
      !count3 = (/nlevgrnd, nlat, nlon /)

      start4 = (/ 1, 1, 1, 1 /)
      count4 = (/ntime, nlevgrnd, nlat, nlon /)

      status = nf_get_var(ncid, pctsand_varid, cellsand, start4, count4)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(cellsand)")

      status = nf_get_var(ncid, pctclay_varid, cellclay, start4, count4)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(cellclay)")

      status = nf_get_var(ncid, orgdens_varid, cellorg, start4, count4)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(cellorg)")

     status = nf_get_var(ncid, watsat_varid, cellwatsat, start4, count4)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(cellwatsat)")

     status = nf_get_var(ncid, watfc_varid, cellwatfc, start4, count4)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(cellwatfc)")

      ! Copy netcdf variables into variables returned by this subroutine.
      ! Eliminate the time dimension. -mdh 9/24/2018
      ! TO DO: make this code piece more efficient!
      do k = 1, nlevgrnd
         do i = 1, nlat
            do j = 1, nlon
               pctsand(j,i,k) = cellsand(j,i,k,1)
               pctclay(j,i,k) = cellclay(j,i,k,1)
               orgdens(j,i,k) = cellorg(j,i,k,1)
               watsat(j,i,k) = cellwatsat(j,i,k,1)
               watfc(j,i,k) = cellwatfc(j,i,k,1)
            end do
         end do
      end do

      status = nf_close(ncid)

      if (verbose .ge. 0) print *, "Done reading surface data from ", trim(nchisth0filename)

   end subroutine readhisth0File

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: readNdepositionFile
!
! !INTERFACE:
   subroutine readNdepositionFile(ncfilename, nlon, nlat, ntime, lon1d, lat1d, time, calyear, ndep)
!
! !DESCRIPTION:
!  Read annual N deposition from netcdf file.  
!  The file I was testing with contained annual values for years 1849-2006.
!
! !USES:
!  none
!
!  !ARGUMENTS:
   character(len=*), intent(in)    :: ncfilename         ! netcdf filename
   integer, intent(out) :: nlon                          ! number of longitudes 
   integer, intent(out) :: nlat                          ! number of latitudes
   integer, intent(out) :: ntime                         ! number of years of data
   ! lon1d, lat1d, and time must be DOUBLE, but ndep is real(4)
   real(8), allocatable, intent(out) :: lon1d(:)         ! lon(lon) (degrees_east)
   real(8), allocatable, intent(out) :: lat1d(:)         ! lat(lat) (degrees_north)
   real(8), allocatable, intent(out) :: time(:)          ! days since 0000-01-01 00:00
   integer, allocatable, intent(out) :: calyear(:)       ! calendar year for each time
   real(4), allocatable, intent(out) :: ndep(:,:,:)      ! ndep(lon, lat, time) Sum of NOy and NHx deposition (gN/m2/yr)

!
! !CALLED FROM:
! subroutine readClimateSequence
!
! !REVISION HISTORY:
! Created by Melannie Hartman
!
!
! !LOCAL VARIABLES:
!EOP

      integer :: ncid                           ! netcdf file ID
      integer :: status                         ! function return status
      integer :: lat_dimid                      ! netcdf dimension id
      integer :: lon_dimid                      ! netcdf dimension id
      integer :: time_dimid                     ! netcdf dimension id
      integer :: lat_varid                      ! netcdf variable id
      integer :: lon_varid                      ! netcdf variable id
      integer :: time_varid                     ! netcdf variable id
      integer :: year_varid                     ! netcdf variable id
      integer :: ndep_varid                     ! netcdf variable id
      integer :: start3(3), count3(3)           ! start and count arrays for reading data from netcdf files
!-----------------------------------------------------------------------

      if (verbose .ge. 0) print *, "Reading N deposition file ", trim(ncfilename), "..."

      status = nf_open(ncfilename, nf_nowrite, ncid)
      if (status /= nf_noerr) call handle_err(status, ncfilename)
         
      ! Get dimension IDs

      status = nf_inq_dimid(ncid, "time", time_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(time)")

      status = nf_inq_dimid(ncid, "lat", lat_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(lat)")

      status = nf_inq_dimid(ncid, "lon", lon_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(lon)")

      ! Get dimension lengths

      status = nf_inq_dimlen(ncid, time_dimid, ntime)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(ntime)")

      status = nf_inq_dimlen(ncid, lat_dimid, nlat)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(lat)")

      status = nf_inq_dimlen(ncid, lon_dimid, nlon)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(lon)")

      ! Get variable IDs

      status = nf_inq_varid(ncid, "lat", lat_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(lat)")

      status = nf_inq_varid(ncid, "lon", lon_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(lon)")

      status = nf_inq_varid(ncid, "time", time_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(time)")

      status = nf_inq_varid(ncid, "YEAR", year_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(YEAR)")

      if (verbose .ge. 1) then
         print *, "  fndep length(lat): ", nlat
         print *, "  fndep length(lon): ", nlon
         print *, "  fndep length(time): ", ntime
      endif

      allocate(lon1d(1:nlon))
      allocate(lat1d(1:nlat))
      allocate(time(1:ntime))
      allocate(calyear(1:ntime))
! Dimensions in FORTRAN are in Column Major Order: the first array index varies the most rapidly.
! Note, in NetCDF file the dimensions appear in the opposite order: time, lat, lon
      allocate(ndep(1:nlon, 1:nlat, 1:ntime))

      lon1d(:) = 0.0
      lat1d(:) = 0.0
      time(:) = 0.0
      calyear(:) = 0
      ndep(:,:,:) = 0.0

      status = nf_get_var(ncid, lat_varid, lat1d)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(LAT1D)")

      status = nf_get_var(ncid, lon_varid, lon1d)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(LON1D)")

      status = nf_get_var(ncid, time_varid, time)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(time)")

      status = nf_inq_varid(ncid, "NDEP_year", ndep_varid)
      if (status /= nf_noerr) call handle_err(status, "NDEP_year")


      ! Retrieve variable values 

      !! Note, calyear could be read directly from variable "YEAR"
      !! calyear(:) = time(:) / DAYS_PER_YEAR
      status = nf_get_var(ncid, year_varid, calyear)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(YEAR)")

      start3 = (/ 1, 1 ,1 /)
      count3 = (/ ntime, nlat, nlon /)

      ! ndep(lon, lat, time) atmospheric N deposition

      status = nf_get_var(ncid, ndep_varid, ndep, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(ndep)")

      status = nf_close(ncid)

      if (verbose .ge. 0) print *, "Done reading N deposition from file ", trim(ncfilename)

   end subroutine readNdepositionFile

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: readDailyHistoryFile
!
! !INTERFACE:
   subroutine readDailyHistoryFile(ncfilename, nlon, nlat, ntime, nlevsoi, nlevgrnd, lon1d, lat1d, &
                                   area, landfrac, landmask, pftmask, time, levgrnd, &
                                   flds, fpsn, fsds, fsdsnd, fsdsni, fsdsvd, fsdsvi, pbot, qbot, &
                                   rain, snow, tbot, thbot, wind, h2osoi, soilliq, tsoi)
!
! !DESCRIPTION:
!  Read latitude, longitude and meteorlogical variables from daily netcdf history files. 
!  It is assumed that each netcdf file contains one year of data.
!
! !USES:
!  none
!
! !ARGUMENTS:
   character(len=*), intent(in)    :: ncfilename  ! netcdf filename
   integer, intent(out) :: nlon                          ! number of longitudes 
   integer, intent(out) :: nlat                          ! number of latitudes
   integer, intent(out) :: ntime                         ! number of hourly time periods per file 
   integer, intent(out) :: nlevsoi                       ! number of soil levels in daily history file
   integer, intent(out) :: nlevgrnd                      ! number of ground levels in daily history file
   real(4), allocatable, intent(out) :: lon1d(:)         ! lon(lon) (degrees_east)
   real(4), allocatable, intent(out) :: lat1d(:)         ! lat(lat) (degrees_north)
   real(4), allocatable, intent(out) :: area(:,:)        ! grid cell areas (km^2)
   real(4), allocatable, intent(out) :: landfrac(:,:)    ! land fraction
   integer, allocatable, intent(out) :: landmask(:,:)    ! land/ocean mask (0=ocean and 1=land)
   integer, allocatable, intent(out) :: pftmask(:,:)     ! pft real/fake mask (0=fake and 1=real)
   real(4), allocatable, intent(out) :: time(:)          ! days since 1901-01-01 00:00:00
   real(4), allocatable, intent(out) :: levgrnd(:)       ! levgrnd(nlevgrnd) coordinate soil levels (m)
   ! All these variables are the mean over the timestep
   real(4), allocatable, intent(out) :: flds(:,:,:)      ! flds(lon, lat, time) atmospheric longwave radiation (W/m^2)
   real(4), allocatable, intent(out) :: fpsn(:,:,:)      ! flds(lon, lat, time) photosynthesis (umol/m2/sec)
   real(4), allocatable, intent(out) :: fsds(:,:,:)      ! fsds(lon, lat, time) atmospheric incident solar radiation (W/m^2)
   real(4), allocatable, intent(out) :: fsdsnd(:,:,:)    ! fsdsnd(lon, lat, time) direct nir incident solar radiation (W/m^2)
   real(4), allocatable, intent(out) :: fsdsni(:,:,:)    ! fsdsni(lon, lat, time) diffuse nir incident solar radiation (W/m^2)
   real(4), allocatable, intent(out) :: fsdsvd(:,:,:)    ! fsdsvd(lon, lat, time) direct vis incident solar radiation" (W/m^2)
   real(4), allocatable, intent(out) :: fsdsvi(:,:,:)    ! fsdsvi(lon, lat, time) diffuse vis incident solar radiation (W/m^2)
   real(4), allocatable, intent(out) :: pbot(:,:,:)      ! pbot(lon, lat, time) atmospheric pressure (Pa)
   real(4), allocatable, intent(out) :: qbot(:,:,:)      ! qbot(lon, lat, time) atmospheric specific humidity (kg/kg)
   real(4), allocatable, intent(out) :: rain(:,:,:)      ! rain(lon, lat, time) atmospheric rain (mm/sec) 
   real(4), allocatable, intent(out) :: snow(:,:,:)      ! snow(lon, lat, time) atmospheric snow (mm/sec) 
   real(4), allocatable, intent(out) :: tbot(:,:,:)      ! tbot(lon, lat, time) atmospheric air temperature (K)
   real(4), allocatable, intent(out) :: thbot(:,:,:)     ! thbot(lon, lat, time) atmospheric air potential temperature (K)
   real(4), allocatable, intent(out) :: wind(:,:,:)      ! wind(lon, lat, time) atmospheric wind velocity magnitude (m/s)
   real(4), allocatable, intent(out) :: h2osoi(:,:,:,:)  ! h2osoi(lon, lat, nlevsoil, time) volumetric soil water (mm3/mm3)(vegetated landunits only) 
   real(4), allocatable, intent(out) :: soilliq(:,:,:,:) ! soilliq(lon, lat, nlevsoil, time) soil liquid water (kg/m2)(vegetated landunits only) 
   real(4), allocatable, intent(out) :: tsoi(:,:,:,:)    ! tsoi(lon, lat, nlevgrnd, time) soil temperature (K) (vegetated landunits only) 

! !CALLED FROM:
! subroutine readClimateSequence
!
! !REVISION HISTORY:
! Created by Melannie Hartman
!
!
! !LOCAL VARIABLES:
!EOP

      integer :: ncid                           ! netcdf file ID
      integer :: status                         ! function return status
      integer :: nlat_dimid                     ! netcdf dimension id
      integer :: nlon_dimid                     ! netcdf dimension id
      integer :: time_dimid                     ! netcdf dimension id
      integer :: nlevsoi_dimid                  ! netcdf dimension id
      integer :: nlevgrnd_dimid                 ! netcdf dimension id
      integer :: lat_varid                      ! netcdf variable id
      integer :: lon_varid                      ! netcdf variable id
      integer :: area_varid                     ! netcdf variable id
      integer :: levgrnd_varid                  ! netcdf variable id
      integer :: landfrac_varid                 ! netcdf variable id
      integer :: landmask_varid                 ! netcdf variable id
      integer :: pftmask_varid                  ! netcdf variable id
      integer :: time_varid                     ! netcdf variable id
      integer :: varid                          ! netcdf variable id
      integer :: start2(2), count2(2)           ! start and count arrays for reading 2-D data from netcdf files
      integer :: start3(3), count3(3)           ! start and count arrays for reading 3-D data from netcdf files
      integer :: start4(4), count4(4)           ! start and count arrays for reading 4-D data from netcdf files
!-----------------------------------------------------------------------

      if (verbose .ge. 0) print *, "Reading daily climate data file ", trim(ncfilename), "..."

      status = nf_open(ncfilename, nf_nowrite, ncid)
      if (status /= nf_noerr) call handle_err(status, ncfilename)
         

      ! Read dimension ids

      status = nf_inq_dimid(ncid, "time", time_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(time)")

      status = nf_inq_dimid(ncid, "lat", nlat_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(lat)")

      status = nf_inq_dimid(ncid, "lon", nlon_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(lon)")

      status = nf_inq_dimid(ncid, "levsoi", nlevsoi_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(levsoi)")

      status = nf_inq_dimid(ncid, "levgrnd", nlevgrnd_dimid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimid(levgrnd)")

      ! Read dimension lengths

      status = nf_inq_dimlen(ncid, time_dimid, ntime)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(time)")

      status = nf_inq_dimlen(ncid, nlat_dimid, nlat)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(lat)")

      status = nf_inq_dimlen(ncid, nlon_dimid, nlon)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(lon)")

      status = nf_inq_dimlen(ncid, nlevsoi_dimid, nlevsoi)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(levsoi)")

      status = nf_inq_dimlen(ncid, nlevgrnd_dimid, nlevgrnd)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_dimlen(levgrnd)")

      ! Read variable IDs

      status = nf_inq_varid(ncid, "lat", lat_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(lat)")

      status = nf_inq_varid(ncid, "lon", lon_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(lon)")

      status = nf_inq_varid(ncid, "area", area_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(area)")

      status = nf_inq_varid(ncid, "landfrac", landfrac_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(landfrac)")

      status = nf_inq_varid(ncid, "landmask", landmask_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(landmask)")

      status = nf_inq_varid(ncid, "pftmask", pftmask_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(pftmask)")

      status = nf_inq_varid(ncid, "time", time_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(time)")

      status = nf_inq_varid(ncid, "levgrnd", levgrnd_varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(levgrnd)")

      if (verbose .ge. 1) then
         print *, "  readDailyHistoryFile: length(lat): ", nlat
         print *, "  readDailyHistoryFile: length(lon): ", nlon
         print *, "  readDailyHistoryFile: length(time): ", ntime
         print *, "  readDailyHistoryFile: length(levsoi): ", nlevsoi
         print *, "  readDailyHistoryFile: length(levgrnd): ", nlevgrnd
      endif

! Dimensions in FORTRAN are in Column Major Order: the first array index varies the most rapidly.
! Note, in NetCDF file the dimensions appear in the opposite order: lat, lon (2-D); time, lat, lon (3-D); time, levgrnd, lat, lon (4-D)
      allocate(lon1d(1:nlon))
      allocate(lat1d(1:nlat))
      allocate(time(1:ntime))
      allocate(levgrnd(1:nlevgrnd))
      allocate(area(1:nlon, 1:nlat))
      allocate(landfrac(1:nlon, 1:nlat))
      allocate(landmask(1:nlon, 1:nlat))
      allocate(pftmask(1:nlon, 1:nlat))
      allocate(flds(1:nlon, 1:nlat, 1:ntime))
      allocate(fpsn(1:nlon, 1:nlat, 1:ntime))
      allocate(fsds(1:nlon, 1:nlat, 1:ntime))
      allocate(fsdsnd(1:nlon, 1:nlat, 1:ntime))
      allocate(fsdsni(1:nlon, 1:nlat, 1:ntime))
      allocate(fsdsvd(1:nlon, 1:nlat, 1:ntime))
      allocate(fsdsvi(1:nlon, 1:nlat, 1:ntime))
      allocate(pbot(1:nlon, 1:nlat, 1:ntime))
      allocate(qbot(1:nlon, 1:nlat, 1:ntime))
      allocate(rain(1:nlon, 1:nlat, 1:ntime))
      allocate(snow(1:nlon, 1:nlat, 1:ntime))
      allocate(tbot(1:nlon, 1:nlat, 1:ntime))
      allocate(thbot(1:nlon, 1:nlat, 1:ntime))
      allocate(wind(1:nlon, 1:nlat, 1:ntime))
      allocate(h2osoi(1:nlon, 1:nlat, 1:nlevsoi, 1:ntime))
      allocate(soilliq(1:nlon, 1:nlat, 1:nlevsoi, 1:ntime))
      allocate(tsoi(1:nlon, 1:nlat, 1:nlevgrnd, 1:ntime))
 
      lon1d(:) = 0.0
      lat1d(:) = 0.0
      time(:) = 0.0
      levgrnd(:) = 0.0

      area(:,:) = 0.0
      landfrac(:,:) = 0.0
      landmask(:,:) = 0
      pftmask(:,:) = 0
 
      flds(:,:,:) = 0.0
      fpsn(:,:,:) = 0.0
      fsds(:,:,:) = 0.0
      fsdsnd(:,:,:) = 0.0
      fsdsni(:,:,:) = 0.0
      fsdsvd(:,:,:) = 0.0
      fsdsvi(:,:,:) = 0.0
      pbot(:,:,:) = 0.0
      qbot(:,:,:) = 0.0
      rain(:,:,:) = 0.0
      snow(:,:,:) = 0.0
      tbot(:,:,:) = 0.0
      thbot(:,:,:) = 0.0
      wind(:,:,:) = 0.0
      !tlai(:,:,:) = 0.0

      h2osoi(:,:,:,:) = 0.0
      soilliq(:,:,:,:) = 0.0
      tsoi(:,:,:,:) = 0.0
 

      status = nf_get_var(ncid, lat_varid, lat1d)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(lat1d)")

      status = nf_get_var(ncid, lon_varid, lon1d)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(lon1d)")

      status = nf_get_var(ncid, time_varid, time)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(time)")

      status = nf_get_var(ncid, levgrnd_varid, levgrnd)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(levgrnd)")

!     do i = 1, nlevgrnd
!         print *, i, levgrnd(i)
!     end do

      start2 = (/ 1 ,1 /)
      count2 = (/ nlat, nlon /)

      ! area(nlon, nlat)

      status = nf_inq_varid(ncid, "area", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(area)")

      status = nf_get_var(ncid, varid, area, start2, count2)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(area)")


      ! landfrac(nlon, nlat)

      status = nf_inq_varid(ncid, "landfrac", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(landfrac)")

      status = nf_get_var(ncid, varid, landfrac, start2, count2)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(landfrac)")


      ! landmask(nlon, nlat)

      status = nf_inq_varid(ncid, "landmask", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(landmask)")

      status = nf_get_var(ncid, varid, landmask, start2, count2)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(landmask)")


      ! pftmask(nlon, nlat)

      status = nf_inq_varid(ncid, "pftmask", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(pftmask)")

      status = nf_get_var(ncid, varid, pftmask, start2, count2)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(pftmask)")


      start3 = (/ 1, 1 ,1 /)
      count3 = (/ ntime, nlat, nlon /)

      ! flds(nlon, nlat, ntime) atmospheric longwave radiation (W/m^2)
 
      if (verbose .ge. 2) print *, "FLDS"
      status = nf_inq_varid(ncid, "FLDS", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(FLDS)")

      status = nf_get_var(ncid, varid, flds, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(flds)")


      ! fpsn(nlon, nlat, ntime) photosynthesis (umol/m2/sec)
      ! Added 8/11/2014
 
      if (verbose .ge. 2) print *, "FPSN"
      status = nf_inq_varid(ncid, "FPSN", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(FPSN)")

      status = nf_get_var(ncid, varid, fpsn, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(fpsn)")


      ! fsds(nlon, nlat, ntime) atmospheric incident solar radiation (W/m^2)

      if (verbose .ge. 2) print *, "FSDS"
      status = nf_inq_varid(ncid, "FSDS", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(FSDS)")

      status = nf_get_var(ncid, varid, fsds, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(fsds)")


      ! fsdsnd(nlon, nlat, ntime) direct nir incident solar radiation (W/m^2)

      if (verbose .ge. 2) print *, "FSDSND"
      status = nf_inq_varid(ncid, "FSDSND", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(FSDSND)")

      status = nf_get_var(ncid, varid, fsdsnd, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(fsdsnd)")


      ! fsdsni(nlon, nlat, ntime) diffuse nir incident solar radiation (W/m^2)

      if (verbose .ge. 2) print *, "FSDSNI"
      status = nf_inq_varid(ncid, "FSDSNI", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(FSDSNI)")

      status = nf_get_var(ncid, varid, fsdsni, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(fsdsni)")


      ! fsdsvd(nlon, nlat, ntime)direct vis incident solar radiation" (W/m^2)

      if (verbose .ge. 2) print *, "FSDSVD"
      status = nf_inq_varid(ncid, "FSDSVD", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(FSDSVD)")

      status = nf_get_var(ncid, varid, fsdsvd, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(fsdsvd)")


      ! fsdsvi(nlon, nlat, ntime) diffuse vis incident solar radiation (W/m^2)

      if (verbose .ge. 2) print *, "FSDSVI"
      status = nf_inq_varid(ncid, "FSDSVI", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(FSDSVI)")

      status = nf_get_var(ncid, varid, fsdsvi, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(fsdsvi)")


      ! pbot(nlon, nlat, ntime) atmospheric pressure (Pa)

      if (verbose .ge. 2) print *, "PBOT"
      status = nf_inq_varid(ncid, "PBOT", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(PBOT)")

      status = nf_get_var(ncid, varid, pbot, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(pbot)")


      ! qbot(nlon, nlat, ntime) atmospheric specific humidity (kg/kg)

      if (verbose .ge. 2) print *, "QBOT"
      status = nf_inq_varid(ncid, "QBOT", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(QBOT)")

      status = nf_get_var(ncid, varid, qbot, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(qbot)")


      ! rain(nlon, nlat, ntime) atmospheric rain (mm/sec)

      if (verbose .ge. 2) print *, "RAIN"
      status = nf_inq_varid(ncid, "RAIN", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(RAIN)")

      status = nf_get_var(ncid, varid, rain, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(rain)")


      ! snow(nlon, nlat, ntime) atmospheric snow (mm/sec)

      if (verbose .ge. 2) print *, "SNOW"
      status = nf_inq_varid(ncid, "SNOW", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(SNOW)")

      status = nf_get_var(ncid, varid, snow, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(snow)")


      ! tbot(nlon, nlat, ntime) atmospheric air temperature (K)

      if (verbose .ge. 2) print *, "TBOT"
      status = nf_inq_varid(ncid, "TBOT", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(TBOT)")

      status = nf_get_var(ncid, varid, tbot, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(tbot)")


      ! thbot(nlon, nlat, ntime) atmospheric air potential temperature (K)

      if (verbose .ge. 2) print *, "THBOT"
      status = nf_inq_varid(ncid, "THBOT", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(THBOT)")

      status = nf_get_var(ncid, varid, thbot, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(thbot)")


      ! wind(nlon, nlat, ntime) atmospheric wind velocity magnitude (m/s)

      if (verbose .ge. 2) print *, "WIND"
      status = nf_inq_varid(ncid, "WIND", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(WIND)")

      status = nf_get_var(ncid, varid, wind, start3, count3)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(wind)")


      ! tlai(nlon, nlat, ntime) total projected LAI

      !if (verbose .ge. 2) print *, "TLAI"
      !status = nf_inq_varid(ncid, "TLAI", varid)
      !if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(TLAI)")
      !
      !status = nf_get_var(ncid, varid, tlai, start3, count3)
      !if (status /= nf_noerr) call handle_err(status, "nf_get_var(tlai)")


      start4 = (/ 1, 1 ,1, 1 /)
      count4 = (/ ntime, nlevsoi, nlat, nlon /)

      ! h2osoi(lon, lat, nlevsoi, time) volumetric soil water content (mm3/mm3)

      if (verbose .ge. 2) print *, "H2OSOI"
      status = nf_inq_varid(ncid, "H2OSOI", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(H2OSOI)")

      status = nf_get_var(ncid, varid, h2osoi, start4, count4)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(h2osoi)")


      ! soilliq(lon, lat, nlevsoi, time) soil liquid water (kg/m2)

      if (verbose .ge. 2) print *, "SOILLIQ"
      status = nf_inq_varid(ncid, "SOILLIQ", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(SOILLIQ)")

      status = nf_get_var(ncid, varid, soilliq, start4, count4)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(soilliq)")


      start4 = (/ 1, 1 ,1, 1 /)
      count4 = (/ ntime, nlevgrnd, nlat, nlon /)

      ! tsoi(lon, lat, nlevgrnd, time) soil temperature (K)

      if (verbose .ge. 2) print *, "TSOI"
      status = nf_inq_varid(ncid, "TSOI", varid)
      if (status /= nf_noerr) call handle_err(status, "nf_inq_varid(TSOI)")

      status = nf_get_var(ncid, varid, tsoi, start4, count4)
      if (status /= nf_noerr) call handle_err(status, "nf_get_var(tsoi)")


      status = nf_close(ncid)

      if (verbose .ge. 0) print *, "Done reading daily climate data from ", trim(ncfilename)

   end subroutine readDailyHistoryFile

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: readClimateSequence
!
! !INTERFACE:
   subroutine readClimateSequence(filesin)
!
! !DESCRIPTION:
!  Read a sequence of monthly, daily, or hourly climate files and store meterological 
!  variables in arrays to drive and create input files for other models.
!  Call readInitFile: Read initialization file, files.ini
!  Call readhisth0File: Read starting monthly history file with surface data 
!  Call readNdepositionFile: Read netcdf N deposition file
!  For daily meteorology:
!  o Call readDailyHistoryFile: Read hourly meteorological variables
!  o Call summarizeDailyVariables: Daily meteorology is aggregated as needed
!  o Call writeCASACNPfiles: Create input files for the CASACNP model  
!  After each year, the arrays are deallocated before reading in the next year's climate.
!
! !USES:
!  use canopy_model
!
!  !ARGUMENTS:
   character(len=*), intent(in)    :: filesin            ! name of file with climate information


   ! LOCAL VARIBLES
   !integer      :: imo, imo1, imo2
   integer       :: nlat, nlon, ntime, ndays
   ! Variables used to create file names
   integer       :: iyr
   character*200 :: ncfilename              ! netcdf climate filename
   character*200 :: nchisth0filename        ! netcdf monthly history file with surface data and nbedrock variables
   character*200 :: ncndepfilename          ! netcdf annual N deposition filename
   character*200 :: filetransCO2            ! name of transient CO2 file (.txt)
   character*200 :: fileClimOutputCSV       ! Name of climate output text file (.csv)
   character*100 :: fileGPPOutputCSV        ! Name of gpp output text file (.csv)
   character*2   :: filetype                ! 'h0' | 'h1'
   !character*2  :: xmonth
   character*4   :: xyear
   integer       :: h                       ! start position of last occurrence of "h0" or "h1" in ncfilename (count starts at 1)
   integer       :: iyear1                  ! first year of climate data to read from ncfilename
   integer       :: imonth1                 ! first month of climate data to read from ncfilename
   integer       :: iday1                   ! first day of climate data to read from ncfilename
   integer       :: iyear2                  ! last year of climate data to read from ncfilename
   integer       :: imonth2                 ! last month of climate data to read from ncfilename
   integer       :: iday2                   ! last day of climate data to read from ncfilename
   integer       :: nFiles                  ! number of NetCDF files to be read
   real(4)       :: time1                   ! time associated with iyear1, imonth1, iday1 (days since 1901-01-01 00:00:00)
   real(4)       :: time2                   ! time associated with iyear2, imonth2, iday2 (days since 1901-01-01 00:00:00)
   ! Meteorological variables stored in NetCDF files (partial time series)
   real(4), allocatable :: lon1d(:)         ! lon(lon) longitude (degrees_east)
   real(4), allocatable :: lat1d(:)         ! lat(lat) latitude (degrees_north)
   real(4), allocatable :: area(:,:)        ! grid cell areas (km^2)
   real(4), allocatable :: landfrac(:,:)    ! land fraction
   integer, allocatable :: landmask(:,:)    ! land/ocean mask (0=ocean and 1=land)
   integer, allocatable :: pftmask(:,:)     ! pft real/fake mask (0=fake and 1=real)
   integer, allocatable :: nbedrock(:,:)    ! index of shallowest bedrock layer
   real(4), allocatable :: time(:)          ! days since 1901-01-01 00:00:00
   real(4), allocatable :: levgrnd(:)       ! coordinate soil levels (m)
   ! All these variables are the mean over the timestep, and time is in hours or days.
   real(4), allocatable :: flds(:,:,:)      ! flds(lon, lat, time) atmospheric longwave radiation (W/m^2)
   real(4), allocatable :: fpsn(:,:,:)      ! fpsn(lon, lat, time) photosynthesis (umol/m2/sec)
   real(4), allocatable :: fsds(:,:,:)      ! fsds(lon, lat, time) atmospheric incident solar radiation (W/m^2)
   real(4), allocatable :: fsdsnd(:,:,:)    ! fsdsnd(lon, lat, time) direct nir incident solar radiation (W/m^2)
   real(4), allocatable :: fsdsni(:,:,:)    ! fsdsni(lon, lat, time) diffuse nir incident solar radiation (W/m^2)
   real(4), allocatable :: fsdsvd(:,:,:)    ! fsdsvd(lon, lat, time) direct vis incident solar radiation" (W/m^2)
   real(4), allocatable :: fsdsvi(:,:,:)    ! fsdsvi(lon, lat, time) diffuse vis incident solar radiation (W/m^2)
   real(4), allocatable :: pbot(:,:,:)      ! pbot(lon, lat, time) atmospheric pressure (Pa)
   real(4), allocatable :: qbot(:,:,:)      ! qbot(lon, lat, time) atmospheric specific humidity (kg/kg)
   real(4), allocatable :: rain(:,:,:)      ! rain(lon, lat, time) atmospheric rain (mm/sec) 
   real(4), allocatable :: snow(:,:,:)      ! snow(lon, lat, time) atmospheric snow (mm/sec) 
   real(4), allocatable :: tbot(:,:,:)      ! tbot(lon, lat, time) atmospheric air temperature (K)
   real(4), allocatable :: thbot(:,:,:)     ! thbot(lon, lat, time) atmospheric air potential temperature (K)
   real(4), allocatable :: wind(:,:,:)      ! wind(lon, lat, time) atmospheric wind velocity magnitude (m/s)
   !real(4), allocatable :: tlai(:,:,:)      ! tlai(lon, lat, time) total projected LAI
   real(4), allocatable :: h2osoi(:,:,:,:)  ! h2osoi(lon, lat, levgrnd, time) volumetric soil water (mm3/mm3)(vegetated landunits only) 
   real(4), allocatable :: soilliq(:,:,:,:) ! soilliq(lon, lat, levgrnd, time) soil liquid water (kg/m2) (vegetated landunits only) 
   real(4), allocatable :: tsoi(:,:,:,:)    ! tsoi(lon, lat, levgrnd, time) soil temperature BY LAYER (K) (vegetated landunits only) 

   ! Surface data set
   integer :: lsmlon                        ! length of longitude dimensions 
   integer :: lsmlat                        ! length of latitude dimension 
   integer :: nlevsoi                       ! number of soil levels (expecting 20)
   integer :: nlevgrnd                      ! number of soil + bedrock levels (expecting 25)
   real(4), allocatable :: pctsand(:,:,:)   ! Percent Sand (lon,lat,nlevgrnd)
   real(4), allocatable :: pctclay(:,:,:)   ! Percent Clay (lon,lat,nlevgrnd)
   real(4), allocatable :: orgdens(:,:,:)   ! organic matter density at soil levels (kg/m3) (lon,lat,nlevgrnd)
   real(4), allocatable :: watsat(:,:,:)    ! (lon,lat,levgrnd)
   real(4), allocatable :: watfc(:,:,:)     ! (lon,lat,levgrnd)

   ! Daily weather and soil water variables
   real(4), allocatable :: tminday(:,:,:)   ! tminday(lon, lat, day) minimum daily air temperature (C)
   real(4), allocatable :: tmaxday(:,:,:)   ! tmaxday(lon, lat, day) maximum daily air temperature (C)
   real(4), allocatable :: rainday(:,:,:)   ! rainday(lon, lat, day) daily atmospheric rain (mm/day) 
   real(4), allocatable :: snowday(:,:,:)   ! snowday(lon, lat, day) daily atmospheric snow (mm/day) 
   real(4), allocatable :: fsdsday(:,:,:)   ! fsdsday(lon, lat, day) 24-hour average atmospheric incident solar radiation (W/m^2)
   real(4), allocatable :: windday(:,:,:)   ! windday(lon, lat, day) 24-hour average atmospheric wind velocity magnitude (m/s)
   real(4), allocatable :: gppday(:,:,:)    ! gppday(lon, lat, day) Gross Primary Production (gC/m2/day)
   real(4), allocatable :: soipsiday(:,:,:) ! soipsiday(lon,lat,day) daily depth-weighted soil water potential(vegetated landunits only) (MPa)
   real(4), allocatable :: h2osoiday(:,:,:,:)     ! h2osoiday(lon, lat, nlevsoi, day) average volumetric liquid soil water BY LAYER (mm3/mm3) (vegetated landunits only) 
   real(4), allocatable :: h2ofrznsoiday(:,:,:,:) ! h2ofrznsoiday(lon, lat, nlevsoi, day) average volumetric frzn soil water BY LAYER (mm3/mm3) (vegetated landunits only) 
!  real(4), allocatable :: soilliqday(:,:,:,:) ! soilliqday(lon, lat, nlevsoi, day) soil liquid water BY LAYER (kg/m2) (vegetated landunits only) 
   real(4), allocatable :: tsoiday(:,:,:,:)    ! tsoiday(lon, lat, nlevgrnd, day) soil temperature BY LAYER (K) (vegetated landunits only) 

   ! Transient CO2
   integer :: y, io
   integer :: ystart                        ! index where co2year(ystart) = iyear, the first year of climate file
   integer :: co2year(MAXCO2YRS)            ! calendar years read from transient CO2 file
   real(4) :: co2ppm(MAXCO2YRS)             ! co2 concentrations (ppm) read from transient CO2 file

   ! Annual N deposition. Time is in years.
   integer :: ndlat, ndlon                  ! ndlat and ndlon should equal nlat, nlon from other NetCDF files
   integer :: ndtime                        ! Number of years in N deposition file
   ! ndep_lon1d, ndep_lat1d, and ndep_time must be DOUBLE, but ndep is real(4)
   real(8), allocatable :: ndep_lon1d(:)    ! lon(lon) longitude (degrees_east)
   real(8), allocatable :: ndep_lat1d(:)    ! lat(lat) latitude (degrees_north)
   real(8), allocatable :: ndep_time(:)     ! days since 0000-01-01 00:00
   integer, allocatable :: ndep_calyear(:)  ! calendar year for each time
   real(4), allocatable :: ndep(:,:,:)      ! ndep(lon, lat, time) annual N deposition (gN/m2/yr)

   !Output .csv file for testing purposes (see subroutine RunCanopyModel)
   if (doWriteDailyGPPTestFile .eq. 1) then
      fileGPPOutputCSV = 'gpp.csv'
      open(unit=16, file=fileGPPOutputCSV)
      write(unit=16, fmt='(11(a7))') 'lat,', 'lon,', 'year,', 'day,', 'daylen,', &
                                     'tmin,', 'tmax,', 'rad,', 'lwpmin,', 'swp,', 'gpp,'
   endif

   call readInitFile(filesin, filetype, ncfilename, nchisth0filename, ncndepfilename, &
                     filetransco2, h, iyear1, imonth1, iday1, iyear2, &
                     imonth2, iday2, time1, time2)
!  call readSurfaceData(ncsrfcfilename, lsmlon, lsmlat, lsmpft, nlevsoi_srfcdata, &
!                       pctpft, pctsand, pctclay, orgdens, watsat, watfc)

   call readhisth0File(nchisth0filename, lsmlon, lsmlat, nlevsoi, nlevgrnd, &
                       nbedrock, pctsand, pctclay, orgdens, watsat, watfc)

   call readNdepositionFile(ncndepfilename, ndlon, ndlat, ndtime, ndep_lon1d, ndep_lat1d, &
                            ndep_time, ndep_calyear, ndep)

   ! Check for consistent grid sizes
   if (ndlat .ne. lsmlat) then
      write(*,*) 'The size of the latitude dimension in ', ncndepfilename, ':', ndlat
      write(*,*) 'does not equal the size of the latitude dimension in ', nchisth0filename, ':', lsmlat
      STOP
   endif
   if (ndlon .ne. lsmlon) then
      write(*,*) 'The size of the longitude dimension in ', ncndepfilename, ':', ndlon
      write(*,*) 'does not equal the size of the longitude dimension in ', nchisth0filename, ':', lsmlon
      STOP
   endif

   !Read transient CO2 file
   open(unit=14, file=trim(filetransco2))
   rewind(unit=14)
   y = 0
   ystart = 0
   co2year(:) = 0
   co2ppm(:) = 0
   do while (y .lt. MAXCO2YRS)
       y = y + 1
       read(unit=14,fmt='(i4,2x,f8.4)',IOSTAT=io) co2year(y), co2ppm(y)
       if (io < 0) exit
       if (co2year(y) .eq. iyear1) ystart = y      ! find position of first climate year
!      print *, y, co2year(y), co2ppm(y)
   end do
!  print*, 'ystart = ', ystart
   close(unit=14)
   if (ystart .eq. 0) then
       print*, iyear1, "was not found in file ", filetransco2
       stop
   endif

   if (filetype .eq. 'h0') then
      write(*,*) 'netcdfTools is not currently configured to read monthly (h0) CLM history files.'
      STOP
   endif

   ! Read data from all the file names in the hourly time series (there is one file per year)
   if (filetype .eq. 'h1') then
       nFiles = (iyear2 - iyear1 + 1) 

       do iyr = iyear1, iyear2

          write(xyear,'(i4)') iyr
          ncfilename(h+3:h+6) = xyear
          if (verbose .ge. 2) print *, iyr, trim(ncfilename)

          if (h1Daily) then
              call readDailyHistoryFile(ncfilename, nlon, nlat, ntime, nlevsoi, nlevgrnd, &
                  lon1d, lat1d, area, landfrac, landmask, pftmask, time, levgrnd, &
                  flds, fpsn, fsds, fsdsnd, fsdsni, fsdsvd, fsdsvi, pbot, qbot, &
                  rain, snow, tbot, thbot, wind, h2osoi, soilliq, tsoi)
          else
              write(*,*) 'netcdfTools is not currently configured to read hourly CLM history files.'
              STOP
          endif


             ! Check for consistent grid sizes
             if (nlat .ne. lsmlat) then
                write(*,*) 'The size of the latitude dimension in ', ncfilename, ':', nlat
                write(*,*) 'does not equal the size of the latitude dimension in ', nchisth0filename, ':', lsmlat
                STOP
             endif
             if (nlon .ne. lsmlon) then
                write(*,*) 'The size of the longitude dimension in ', ncfilename, ':', nlon
                write(*,*) 'does not equal the size of the longitude dimension in ', nchisth0filename, ':', lsmlon
                STOP
             endif

          if (h1Daily) then
              ! Summarize the daily variables in the clm daily history file (mostly soil variables).
              ! Daily clm history files contain fpsn (used to compute daily GPP).  
              ! No need to call aggregated canopy model to compute GPP.

              call summarizeDailyVariables(nlon, nlat, ntime, nlevsoi, nlevgrnd, &
                  lon1d, lat1d, time, levgrnd, &
                  flds, fpsn, fsds, fsdsnd, fsdsni, fsdsvd, fsdsvi, pbot, qbot, &
                  rain, snow, tbot, thbot, wind, h2osoi, soilliq, tsoi, &
                  pctsand, pctclay, orgdens, &
                  tminday, tmaxday, rainday, snowday, fsdsday, windday, &
                  gppday, soipsiday, h2osoiday, h2ofrznsoiday, tsoiday)

                  ndays = ntime
          else

              write(*,*) 'netcdfTools is not currently configured to read hourly CLM history files.'
              STOP

!             ! Run the aggregated canopy model
!   
!             if (verbose .ge. 0) print *, "Running Canopy Model for year ", iyr
!             if (co2year(ystart) .ne. iyr) then
!                 print*, "There is no CO2 concentration for year ", iyr
!                 stop
!             endif
!             ndays = 365
!             call RunCanopyModel(iyr, nlon, nlat, ndays, lon1d, lat1d, &
!                 area, landfrac, landmask, pftmask, &
!                 tminday, tmaxday, fsdsday, tlaiday, soipsiday, co2ppm(ystart), gppday)
!             ystart = ystart+1
    
           endif

          ! Create input files for the CASACNP model
     
          call writeCASACNPfiles(iyr, iyear1, iyear2, nlon, nlat, &
              nlevsoi, nlevgrnd, ndays, ndtime, ndep_calyear, &
              lon1d, lat1d, levgrnd, area, landfrac, landmask, pftmask, &
              ndep, tminday, tmaxday, gppday, &
              h2osoiday, h2ofrznsoiday, tsoiday, pctsand, pctclay, &
              watsat, watfc, nbedrock)

          !Deallocate all variables that are read from the daily climate file each year 

          deallocate(lon1d, lat1d, area, landfrac, landmask, pftmask, time, levgrnd, &
             flds, fpsn, fsds, fsdsnd, fsdsni, fsdsvd, fsdsvi, &
             pbot, qbot, rain, snow, tbot, thbot, wind, h2osoi, soilliq, tsoi, &
             tminday, tmaxday, rainday, snowday, fsdsday, windday, gppday, &
             soipsiday, h2osoiday, h2ofrznsoiday, tsoiday)

       end do
   endif
  
   if (doWriteDailyGPPTestFile .eq. 1) close(unit=16)

   deallocate(pctsand, pctclay, orgdens, watsat, watfc, nbedrock)

   end subroutine readClimateSequence

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: readInitFile
!
! !INTERFACE:
   subroutine readInitFile(filesin, filetype, ncfilename, nchisth0filename, &
                           ncndepfilename, filetransCO2, hindex, &
                           iyear1, imonth1, iday1, iyear2, imonth2, iday2, time1, time2)
!
! !DESCRIPTION:
!  Read the initialization file (filesin).  Retrieve the following from filesin:
!    climate file file type (line 1)
!    starting and ending dates (lines 2 and 3)
!    path and file name of one of the netcdf climate files (line 4)*  
!    exact path and name of starting monthly history file (line 5)
!    exact path and name of N deposition file (line 6)
!    exact path and name of transient CO2 file name (line 7)
!
!  *The year and month in the climate file name will be substituted according to start and end dates.
!
!  Example file format:
!
!  h1         ! file type (h1=daily)
!  19010101   ! start (yyyymmdd)
!  19011231   ! end (yyyymmdd)
!  /project/tss/wwieder/CASACLM/clm_forcing/CLM5sp_GSWP3_hist/CLM5sp_HIST_GSWP3.clm2.h1.1901-01-01-00000.nc
!  /project/tss/wwieder/CASACLM/clm_forcing/CLM5sp_GSWP3_hist/CLM5sp_HIST_GSWP3.clm2.h0.1850-01.nc
!  /project/tss/wwieder/CASACLM/NetCDFTools/fndep_clm_rcp8.5_simyr1849-2106_1.9x2.5_c100428.nc
!  ./CO2_1768-2100.txt
!
! !USES:
!  none
!
!  !ARGUMENTS:
   character(len=*), intent(in)    :: filesin            ! name of file with climate information
   character(len=*), intent(out)   :: filetype           ! type of climate file to read: 'h0' (monthly) or 'h1' (daily or hourly)
   character(len=*), intent(out)   :: ncfilename         ! name of first climate netcdf climate file to read
   character(len=*), intent(out)   :: nchisth0filename   ! name of monthly history file with surface data and nbedrock variable to read
   character(len=*), intent(out)   :: ncndepfilename     ! name of annual N deposition netcdf file
   character(len=*), intent(out)   :: filetransCO2       ! name of transient CO2 file (.txt)
   integer, intent(out)            :: hindex             ! start position of last occurrence of "h0" or "h1" in ncfilename (count starts at 1)
   integer, intent(out)            :: iyear1             ! first year of climate data to read from ncfilename
   integer, intent(out)            :: imonth1            ! first month of climate data to read from ncfilename
   integer, intent(out)            :: iday1              ! first day of climate data to read from ncfilename
   integer, intent(out)            :: iyear2             ! last year of climate data to read from ncfilename
   integer, intent(out)            :: imonth2            ! last month of climate data to read from ncfilename
   integer, intent(out)            :: iday2              ! last day of climate data to read from ncfilename
   real(4), intent(out)            :: time1              ! time associated with iyear1, imonth1, iday1 (days since 1901-01-01 00:00:00)
   real(4), intent(out)            :: time2              ! time associated with iyear2, imonth2, iday2 (days since 1901-01-01 00:00:00)

!  !LOCAL VARIABLES

   character*8   :: start_date, end_date        ! yyyymmdd
   character*4   :: start_year, end_year        ! yyyy
   character*2   :: start_month, end_month      ! mm
   character*2   :: start_day, end_day          ! dd
   logical       :: back                        ! used as argument to index function
   integer       :: ihour1, ihour2              ! 1..24
   integer       :: idoy1, idoy2                ! 1 - 365
   integer       :: doy(0:12)                   ! last day of each month (day of year)

   if (verbose .ge. 0) print *, "Reading initialization file ", trim(filesin), "..."
   open(unit=10, file=trim(filesin))
   rewind(unit=10)
   read(unit=10, fmt='(a2)') filetype           ! 'h0' or 'h1'
   read(unit=10, fmt='(a8)') start_date         ! yyyymmdd
   read(unit=10, fmt='(a8)') end_date           ! yyyymmdd
   read(unit=10, fmt='(a200)') ncfilename
   read(unit=10, fmt='(a200)') nchisth0filename
   read(unit=10, fmt='(a200)') ncndepfilename
   read(unit=10, fmt='(a200)') filetransCO2
   ncfilename = trim(ncfilename)
   nchisth0filename = trim(nchisth0filename)
   ncndepfilename = trim(ncndepfilename)
   filetransCO2 = trim(filetransCO2)
   close(unit=10)

   back = .true.
   hindex = index(ncfilename, filetype, back)   ! start position of last occurrence of "h0" or "h1" in ncfilename (count starts at 1)
   if (hindex .eq. 0) then
       write(*,*) 'Substring ', filetype, ' was not found in netCDF file name ' , ncfilename
       STOP
   endif
   if (verbose .ge. 2) then
      print *, 'index = ', hindex
      print *, 'start_date = ', start_date
      print *, 'end_date = ', end_date
   endif
  
   doy(0) = 0
   doy(1) = doy(0) + 31
   doy(2) = doy(1) + 28
   doy(3) = doy(2) + 31
   doy(4) = doy(3) + 30
   doy(5) = doy(4) + 31
   doy(6) = doy(5) + 30
   doy(7) = doy(6) + 31
   doy(8) = doy(7) + 31
   doy(9) = doy(8) + 30
   doy(10) = doy(9) + 31
   doy(11) = doy(10) + 30
   doy(12) = doy(11) + 31

   ! INSERT RANGE CHECKING ON imonth1, imonth2

   start_year = start_date(1:4)
   start_month = start_date(5:6)
   start_day = start_date(7:8)
   end_year = end_date(1:4)
   end_month = end_date(5:6)
   end_day = end_date(7:8)
   if (verbose .ge. 1) then
      print *, '  start_year = ', start_year
      print *, '  start_month = ', start_month
      print *, '  start_day = ', start_day
      print *, '  end_year = ', end_year
      print *, '  end_month = ', end_month
      print *, '  end_day = ', end_day
   endif

   ! Copy string variables (start_*, end_*) into integer variables
   read(start_year, '(i4)') iyear1
   read(start_month, '(i4)') imonth1
   read(start_day,  '(i4)') iday1
   read(end_year, '(i4)') iyear2
   read(end_month, '(i4)') imonth2
   read(end_day,  '(i4)') iday2

   ihour1 = 1
   ihour2 = 24
   idoy1 = doy(imonth1-1) + iday1 
   idoy2 = doy(imonth2-1) + iday2 
   if (filetype .eq. 'h0') then
      ihour1 = 24
      ihour2 = 24
      idoy1 = doy(imonth1) 
      idoy2 = doy(imonth2) 
   endif
   
   call getTime(iyear1, idoy1, ihour1, time1)
   call getTime(iyear2, idoy2, ihour2, time2)

   if (verbose .ge. 2) then
      print *, 'time1 = ', time1
      print *, 'time2 = ', time2
   endif

   if (verbose .ge. 0) print *, "Done reading initialization file ", trim(filesin)
  
   end subroutine readInitFile

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: summarizeDailyVariables
!
! !INTERFACE:
   subroutine summarizeDailyVariables(nlon, nlat, ntimes, nlevsoi, nlevgrnd, &
                                 lon1d, lat1d, time, levgrnd, &
                                 flds, fpsn, fsds, fsdsnd, fsdsni, fsdsvd, fsdsvi, pbot, &
                                 qbot, rain, snow, tbot, thbot, wind, &
                                 h2osoi, soilliq, tsoi, pctsand, pctclay, orgdens, &
                                 tminday, tmaxday, rainday, snowday, fsdsday, windday, &
                                 gppday, soipsiday, h2osoiday, h2ofrznsoiday, tsoiday)

! !DESCRIPTION:
!  Average and aggregate daily weather variables to compute daily weather variables.
!  Compute daily depth-weighted soil water potential from soilliq (not h2osoi), pctsand, pctclay, and orgdens.
!
! !USES:
!  none
!
!  !ARGUMENTS:
   integer, intent(in) :: nlon             ! number of longitudes 
   integer, intent(in) :: nlat             ! number of latitudes
   integer, intent(in) :: ntimes           ! total number of hourly time periods (8760 hours - 1 year)
   integer, intent(in) :: nlevsoi          ! number of soil levels from history file
   integer, intent(in) :: nlevgrnd         ! number of soil + bedrock levels from history file
   real(4), intent(in) :: lon1d(:)         ! longitude(lon) (degrees_east)
   real(4), intent(in) :: lat1d(:)         ! latitude(lat) (degrees_north)
   real(4), intent(in) :: time(:)          ! days since 1901-01-01 00:00:00
   real(4), intent(in) :: levgrnd(:)       ! levgrnd(nlevgrnd)coordinate soil levels from daily history file (m)
   real(4), intent(in) :: flds(:,:,:)      ! flds(lon, lat, time) atmospheric longwave radiation (W/m^2)
   real(4), intent(in) :: fpsn(:,:,:)      ! fpsn(lon, lat, time) photosynthesis (umol/m2/sec)
   real(4), intent(in) :: fsds(:,:,:)      ! fsds(lon, lat, time) atmospheric incident solar radiation (W/m^2)
   real(4), intent(in) :: fsdsnd(:,:,:)    ! fsdsnd(lon, lat, time) direct nir incident solar radiation (W/m^2)
   real(4), intent(in) :: fsdsni(:,:,:)    ! fsdsni(lon, lat, time) diffuse nir incident solar radiation (W/m^2)
   real(4), intent(in) :: fsdsvd(:,:,:)    ! fsdsvd(lon, lat, time) direct vis incident solar radiation" (W/m^2)
   real(4), intent(in) :: fsdsvi(:,:,:)    ! fsdsvi(lon, lat, time) diffuse vis incident solar radiation (W/m^2)
   real(4), intent(in) :: pbot(:,:,:)      ! pbot(lon, lat, time) atmospheric pressure (Pa)
   real(4), intent(in) :: qbot(:,:,:)      ! qbot(lon, lat, time) atmospheric specific humidity (kg/kg)
   real(4), intent(in) :: rain(:,:,:)      ! rain(lon, lat, time) atmospheric rain (mm/sec) 
   real(4), intent(in) :: snow(:,:,:)      ! snow(lon, lat, time) atmospheric snow (mm/sec) 
   real(4), intent(in) :: tbot(:,:,:)      ! tbot(lon, lat, time) atmospheric air temperature (K)
   real(4), intent(in) :: thbot(:,:,:)     ! thbot(lon, lat, time) atmospheric air potential temperature (K)
   real(4), intent(in) :: wind(:,:,:)      ! wind(lon, lat, time) atmospheric wind velocity magnitude (m/s)
   real(4), intent(in) :: h2osoi(:,:,:,:)  ! h2osoi(lon, lat, nlevsoi, time) daily volumetric soil water (mm3/mm3)(vegetated landunits only) 
   real(4), intent(in) :: soilliq(:,:,:,:) ! soilliq(lon, lat, nlevsoi, time) daily soil liquid water (kg/m2)(vegetated landunits only) 
   real(4), intent(in) :: tsoi(:,:,:,:)    ! tsoi(lon, lat, nlevgrnd, time) daily soil temperature (K) (vegetated landunits only) 
   real(4), intent(in) :: pctsand(:,:,:)   ! cellsand(lon, lat, nlevgrnd, time) percent sand
   real(4), intent(in) :: pctclay(:,:,:)   ! cellclay(lon, lat, nlevgrnd, time) percent clay
   real(4), intent(in) :: orgdens(:,:,:)   ! cellorg(lon, lat, nlevgrnd, time) organic matter density (kg/m3)

   !Daily weather and soil variables
   real(4), allocatable, intent(out) :: tminday(:,:,:)      ! tminday(lon, lat, day) minimum daily air temperature (C)
   real(4), allocatable, intent(out) :: tmaxday(:,:,:)      ! tmaxday(lon, lat, day) maximum daily air temperature (C)
   real(4), allocatable, intent(out) :: rainday(:,:,:)      ! rainday(lon, lat, day) daily atmospheric rain (mm/day)
   real(4), allocatable, intent(out) :: snowday(:,:,:)      ! snowday(lon, lat, day) daily atmospheric snow (mm/day)
   real(4), allocatable, intent(out) :: fsdsday(:,:,:)      ! fsdsday(lon, lat, day) 24-hour average atmospheric incident solar radiation (W/m^2)
   real(4), allocatable, intent(out) :: windday(:,:,:)      ! windday(lon, lat, day) 24-hour average atmospheric wind velocity magnitude (m/s)
   real(4), allocatable, intent(out) :: gppday(:,:,:)       ! gppday(lon, lat, day) daily GPP (gC/m2/day)
   real(4), allocatable, intent(out) :: soipsiday(:,:,:)    ! soipsiday(lon, lat, day) depth-weighted soil water potential (MPa) (vegetated landunits only) 
   real(4), allocatable, intent(out) :: h2osoiday(:,:,:,:)  ! h2osoiday(lon, lat, nlevsoi, day) average liquid volumetric soil water BY LAYER (mm3/mm3) (vegetated landunits only) 
   real(4), allocatable, intent(out) :: h2ofrznsoiday(:,:,:,:) ! h2ofrznsoiday(lon, lat, nlevsoi, day) average frozen volumetric soil water BY LAYER (mm3/mm3) (vegetated landunits only) 
!  real(4), allocatable, intent(out) :: soilliqday(:,:,:,:) ! soilliqday(lon, lat, nlevsoi, day) average soil liquid water BY LAYER (kg/m2) (vegetated landunits only) 
   real(4), allocatable, intent(out) :: tsoiday(:,:,:,:)    ! tsoiday(lon, lat, nlevgrnd, day) soil temperature BY LAYER (K) (vegetated landunits only) 

   ! LOCAL VARIABLES
   integer :: i, j, k, day
   integer ::  calyear, dayofyr, month, dayofmo, hour  ! needed only to compute calyear for test output file
   character*200 :: fileDailyClimOutputCSV      ! Name of output file (.csv)
   character*1   :: c                           ! delimeter (a comma)
   integer :: tbotMissing                       ! set to 1 if missing hourly tbot value, reset to 0 each day
   integer :: rainMissing                       ! set to 1 if missing hourly rain value, reset to 0 each day
   integer :: snowMissing                       ! set to 1 if missing hourly snow value, reset to 0 each day
   integer :: fsdsMissing                       ! set to 1 if missing hourly fsds value, reset to 0 each day
   integer :: windMissing                       ! set to 1 if missing hourly wind value, reset to 0 each day
   !integer :: tlaiMissing                       ! set to 1 if missing hourly tlai value, reset to 0 each day
   integer :: gppMissing                        ! set to 1 if missing hourly fpsn value, reset to 0 each day
   integer, allocatable :: h2osoiMissing(:)     ! BY LAYER, set to 1 if missing daily h2osoi value, reset to 0 each day
   integer, allocatable :: soilliqMissing(:)    ! BY LAYER, set to 1 if missing daily soilliq value, reset to 0 each day
   integer, allocatable :: tsoiMissing(:)       ! BY LAYER, set to 1 if missing daily tsoi value, reset to 0 each day
   character*4   :: xyear                       ! string version of calyear
   integer :: lyr50cm                           ! soil layer with lower boundary ~50cm (0.50 m)
   real(4) :: levlower50cm                      ! lower boundary of soil layer lyr50cm (m)
   real(4) :: levlower, levupper                ! lower and upper boundaries of soil layer with center at levgrnd(i) (m)
   real(4) :: soipsi                            ! Intermediate calculation for soil water potential (MPa)
   real(4) :: sand, clay, org                   ! Temporary variables for soipsi calculation
   real(4), allocatable :: levgrndthickness(:)  ! levgrndthickness(nlevsoi+1) thickness of soil layers (m)
  

   if (verbose .ge. 0) print *, "Summarizing daily variables..."

   !----------------------------------------------------------------------------------------------------
   ! Write weather variables to .csv file for testing purposes
   !----------------------------------------------------------------------------------------------------
   calyear = 0
   if (ntimes .gt. 1) call getYearMonthDayHour(time(ntimes-1), calyear, dayofyr, month, dayofmo, hour)
   write(xyear,'(i4)') calyear
   if (doWriteDailyClimateTestFile .eq. 1) then
      fileDailyClimOutputCSV = 'dailyClim_' // xyear // '.csv'
      open(unit=12, file=trim(fileDailyClimOutputCSV))
      c = ','
      write(unit=12,fmt='(13(a12))') 'lon,', 'lat,', 'year,', 'day,', & 
         'tmin(C),', 'tmax(C),', 'rain(mm),', 'snow(mm),', 'fsds(W/m2),', &
         'wind(m/s),', 'gpp(gC/m2)', 'soipsi(MPa)'
   endif
   !----------------------------------------------------------------------------------------------------

   allocate(tminday(1:nlon, 1:nlat, 1:365))
   allocate(tmaxday(1:nlon, 1:nlat, 1:365))
   allocate(rainday(1:nlon, 1:nlat, 1:365))
   allocate(snowday(1:nlon, 1:nlat, 1:365))
   allocate(fsdsday(1:nlon, 1:nlat, 1:365))
   allocate(windday(1:nlon, 1:nlat, 1:365))
   allocate(gppday(1:nlon, 1:nlat, 1:365))

   tminday(:,:,:) = 1000
   tmaxday(:,:,:) = -1000
   rainday(:,:,:) = 0
   snowday(:,:,:) = 0
   fsdsday(:,:,:) = 0
   windday(:,:,:) = 0
   gppday(:,:,:) = 0

   !------------------------------------------------------------------------------------------
   ! Compute  soil water potential by layer and  depth-weighted average of soil water potential 
   ! from volumetric soil water content, pctsand, pctclay, and orgdens.
   ! ATTENTION - calculate for top 50 cm soil levels only
   !------------------------------------------------------------------------------------------


   !h2osoiday and h2ofrznsoiday need to be dimensioned for 1:nlevsoi (1:20) for casacnp
   !tsoiday needs to be dimensioned for 1:nlevgrnd (1:25) for casacnp

   allocate(h2osoiday(1:nlon, 1:nlat, 1:nlevsoi, 1:365))           !This calculation is done by layer
   allocate(h2ofrznsoiday(1:nlon, 1:nlat, 1:nlevsoi, 1:365))       !This calculation is done by layer
!  allocate(soilliqday(1:nlon, 1:nlat, 1:nlevsoi, 1:365))          !This calculation is done by layer
   allocate(tsoiday(1:nlon, 1:nlat, 1:nlevgrnd, 1:365))            !This calculation is done by layer
   allocate(soipsiday(1:nlon, 1:nlat, 1:365))                      !This calculation is depth-weighted average 
   allocate(levgrndthickness(1:nlevsoi+1))
   allocate(h2osoiMissing(1:nlevsoi))
   allocate(soilliqMissing(1:nlevsoi))
   allocate(tsoiMissing(1:nlevgrnd))

   levgrndthickness(:) = 0.0
   h2osoiday(:,:,:,:) = 0
   h2ofrznsoiday(:,:,:,:) = 0
!  soilliqday(:,:,:,:) = 0
   tsoiday(:,:,:,:) = 0
   soipsiday(:,:,:) = 0

   levlower = 0
   levupper = 0
   levlower50cm = 0.0  ! Depth of soil (m) for the soil layer closest to 50cm. This will be reset below. -mdh 2/21/2017
   do k = 1, nlevsoi
      levlower = levgrnd(k) + (levgrnd(k+1) - levgrnd(k))/2
      levgrndthickness(k) = levlower - levupper
      levupper = levlower
      if (verbose .ge. 2) print *, k, levgrndthickness(k)
   end do
   lyr50cm = 0
   k = 0
   do while (lyr50cm == 0) 
      k = k + 1
      levlower = levgrnd(k) + (levgrnd(k+1) - levgrnd(k))/2
      if (levlower >= 0.49) then
          levlower50cm = levlower
          lyr50cm = k
      endif
   end do

   write(*,*) 'lyr50cm =', lyr50cm
   write(*,*) 'levlower50cm =', levlower50cm

   !------------------------------------------------------------------------------------------
   ! Compute daily values
   !------------------------------------------------------------------------------------------

   do i = 1, nlat
      do j = 1, nlon
         do day = 1, 365
   
            tbotMissing = 0
            rainMissing = 0
            snowMissing = 0
            fsdsMissing = 0
            windMissing = 0
            !tlaiMissing = 0
            h2osoiMissing(:) = 0
            soilliqMissing(:) = 0
            tsoiMissing(:) = 0
            gppMissing = 0
   
            if (tbot(j,i,day) .lt. MISSING_VALUE) then
               if (tbot(j,i,day) .lt. tminday(j,i,day) + KELVIN) tminday(j,i,day) = tbot(j,i,day) - KELVIN
               if (tbot(j,i,day) .gt. tmaxday(j,i,day) + KELVIN) tmaxday(j,i,day) = tbot(j,i,day) - KELVIN
            else
               tbotMissing = 1
            endif
   
            if (rain(j,i,day) .lt. MISSING_VALUE) then
               rainday(j,i,day) = rain(j,i,day) * SEC_PER_DAY 
            else
               rainMissing = 1
            endif
   
            if (snow(j,i,day) .lt. MISSING_VALUE) then
               snowday(j,i,day) = snow(j,i,day) * SEC_PER_DAY
            else
               snowMissing = 1
            endif
   
            if (fsds(j,i,day) .lt. MISSING_VALUE) then
               fsdsday(j,i,day) = fsds(j,i,day)
            else
               fsdsMissing = 1
            endif
   
            if (wind(j,i,day) .lt. MISSING_VALUE) then
               windday(j,i,day) = wind(j,i,day) 
            else
               windMissing = 1
            endif
   
            !if (tlai(j,i,day) .lt. MISSING_VALUE) then
            !   tlaiday(j,i,day) = tlai(j,i,day)
            !else
            !   tlaiMissing = 1
            !endif
   
            if (fpsn(j,i,day) .lt. MISSING_VALUE) then
               ! Convert umol/m2/sec to gC/m2/day
               gppday(j,i,day) = fpsn(j,i,day) * 12.0 * MOL_PER_UMOL * SEC_PER_DAY
            else
               gppMissing = 1
            endif
   
            do k = 1, nlevsoi 
               if (soilliq(j,i,k,day) .lt. MISSING_VALUE) then
                  ! Compute volumentric soil water content (mm3/mm3) from soil liquid water (kg/m2) -mdh 2/20/2017
                  h2osoiday(j,i,k,day) = soilliq(j,i,k,day) * 0.001 / levgrndthickness(k)
                  ! Compute the frozen fraction also. -mdh 3/13/2017
                  h2ofrznsoiday(j,i,k,day) = h2osoi(j,i,k,day) - h2osoiday(j,i,k,day)
                  if (h2ofrznsoiday(j,i,k,day) < 0.0) then
                     !write(*,*) 'WARNING: h2ofrznsoiday(',j,i,k,day,') < 0:', h2ofrznsoiday(j,i,k,day)
                     !write(*,*) '  Resetting h2ofrznsoiday to 0.0'
                     h2ofrznsoiday(j,i,k,day) = 0.0
                  endif
                  !write(*,*)
                  !write(*,*) 'soilliq(',j,i,k,day, ') =', soilliq(j,i,k,day)
                  !write(*,*) 'h2osoiday(',j,i,k,day, ') =', h2osoiday(j,i,k,day)
                  !write(*,*) 'h2ofrznsoiday(',j,i,k,day, ') =', h2ofrznsoiday(j,i,k,day)
                  !write(*,*) 'levgrndthickness(', k, ') =', levgrndthickness(k)
               else
                  h2osoiMissing(k) = 1
                  soilliqMissing(k) = 1
               end if
            end do
   
            do k = 1, nlevsoi 
               if (tsoi(j,i,k,day) .lt. MISSING_VALUE) then
                  tsoiday(j,i,k,day) = tsoi(j,i,k,day) 
               else
                  tsoiMissing(k) = 1
               end if
            end do
   
            if (tbotMissing .eq. 1) tminday(j,i,day) = MISSING_VALUE
            if (tbotMissing .eq. 1) tmaxday(j,i,day) = MISSING_VALUE
            if (rainMissing .eq. 1) rainday(j,i,day) = MISSING_VALUE
            if (snowMissing .eq. 1) snowday(j,i,day) = MISSING_VALUE
            if (fsdsMissing .eq. 1) fsdsday(j,i,day) = MISSING_VALUE
            if (windMissing .eq. 1) windday(j,i,day) = MISSING_VALUE
            !if (tlaiMissing .eq. 1) tlaiday(j,i,day) = MISSING_VALUE
            if (gppMissing .eq. 1) gppday(j,i,day) = MISSING_VALUE
            do k = 1, nlevsoi
               if (h2osoiMissing(k) .eq. 1) h2osoiday(j,i,k,day) = MISSING_VALUE
               if (soilliqMissing(k) .eq. 1) h2osoiday(j,i,k,day) = MISSING_VALUE
               if (h2osoiMissing(k) .eq. 1 .or. soilliqMissing(k) .eq. 1) h2ofrznsoiday(j,i,k,day) = MISSING_VALUE
               if (tsoiMissing(k) .eq. 1) tsoiday(j,i,k,day) = MISSING_VALUE
            end do
         enddo
      enddo
   enddo

   !------------------------------------------------------------------------------------------
   ! Compute soil water potential by layer and depth-weighted average of soil water  
   ! potential from volumetric soil water content, pctsand, pctclay, and orgdens.
   ! Depth-weighted averages include the top 50 cm only.
   !------------------------------------------------------------------------------------------

   do day = 1, 365
      do k = 1, lyr50cm
         do i = 1, nlat
            do j = 1, nlon
               if (h2osoiday(j,i,k,day) .lt. MISSING_VALUE) then
                  sand = REAL(pctsand(j,i,k))
                  clay = REAL(pctclay(j,i,k))
                  org = REAL(orgdens(j,i,k))
                  soipsi = soilWaterPotential(h2osoiday(j,i,k,day), sand, clay, org, levgrnd(k))
                  soipsiday(j,i,day) = soipsiday(j,i,day) + soipsi * levgrndthickness(k)
               else
                  soipsiday(j,i,day) = MISSING_VALUE
               end if
            end do
         end do
      end do
   end do

   do day = 1, 365
      do i = 1, nlat
         do j = 1, nlon
            if (soipsiday(j,i,day) .lt. MISSING_VALUE) then
               soipsiday(j,i,day) = soipsiday(j,i,day) / levlower50cm
            end if
         end do
      end do
   end do

   !------------------------------------------------------------------------------------------
   ! Write to daily weather to .csv file for testing purposes.
   if (doWriteDailyClimateTestFile .eq. 1) then
      do j = LONINDX,LONINDX
         do i = LATINDX,LATINDX
            do day = 1,365
               write(unit=12, fmt='(2(f0.2,a1), 2(i6,a1), 9(f0.6,a1))') &
                   lon1d(j),c, lat1d(i),c, calyear,c, day,c, &
                   tminday(j,i,day),c, tmaxday(j,i,day),c, rainday(j,i,day),c, snowday(j,i,day),c, &
                   fsdsday(j,i,day),c, windday(j,i,day),c, gppday(j,i,day),c, soipsiday(j,i,day),c
                   !fsdsday(j,i,day),c, windday(j,i,day),c, tlaiday(j,i,day),c, gppday(j,i,day),c, soipsiday(j,i,day),c
            enddo
         enddo
      enddo
      close(unit=12)
   endif

   if (verbose .ge. 0) print *, "Done summarizing daily variables"

   end subroutine summarizeDailyVariables

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: soilWaterPotential
!
! !INTERFACE:
   real function soilWaterPotential(h2osoi, sand, clay, org, zsoi)
!
! !DESCRIPTION:
!  Computes soil water potential (MPa).
!  Reference: See CLM4 Technical Notes, eq. 7.85, p. 141 (some modifications have been made)
!
! !USES:
!  none
!
!  !ARGUMENTS:
   real(4), intent(in) :: h2osoi    ! volumetric soil water content (mm3/mm3)
   real(4), intent(in) :: sand      ! percent sand (0-100), from surface dataset
   real(4), intent(in) :: clay      ! percent clay (0-100), from surface dataset
   real(4), intent(in) :: org       ! organic matter density (kg/m3), from surface dataset
   real(4), intent(in) :: zsoi      ! soil level (m), from surface dataset

!  !LOCAL VARIABLES:

   ! Mineral soil:
   real(4) :: Watsat           ! volumetric soil water at saturation (porosity)
   real(4) :: bsw              ! Clapp and Hornberger "b"
   real(4) :: sucsat           ! Saturated soil water suction (mm)

   ! Organic soil:
   real(4) :: om_watsat        ! porosity of organic soil
   real(4) :: om_b             ! Clapp Hornberger paramater for organic soil
   real(4) :: om_sucsat        ! saturated suction for organic matter (mm)
   real(4) :: zsapric          ! depth (m) that organic matter takes on
   real(4) :: om_frac          ! organic matter fraction

   ! Soil water potential:
   real(4) :: head             ! Head of pressure (MPa/m)
   real(4) :: s_node           ! water content relative to saturation
   real(4) :: smp_mm           ! soil water potential (mm)
   real(4) :: smp_mpa          ! soil water potential (MPa)

   Watsat = 0.489 - 0.00126 * sand
   bsw    = 2.91 + 0.159 * clay
   sucsat = 10.0 * ( 10.0**(1.88 - 0.0131 * sand) )

   !Characteristics of sapric peat
   zsapric    = 0.5 
   om_watsat  = max(0.93 - 0.1 * (zsoi/zsapric), 0.83)
   om_b       = min(2.7  + 9.3 * (zsoi/zsapric), 12.0)
   om_sucsat  = min(10.3 - 0.2 * (zsoi/zsapric), 10.1)

   !This is the combined mineral and organic values:
  
   om_frac = org / ORGANIC_MAX
   Watsat  = (1.0 - om_frac)*Watsat + om_watsat*om_frac
   bsw     = (1.0 - om_frac)*bsw    + om_b*om_frac
   sucsat  = (1.0 - om_frac)*sucsat + om_sucsat*om_frac

   s_node = h2osoi / Watsat
   smp_mm = -sucsat * s_node**(-bsw)

   !Convert soil water potential from mm to MPa

   head = DENH2O * GRAV * 1.e-06        ! Head of pressure  (MPa/m)
   smp_mpa = smp_mm * 1.e-03 * head     ! mm -> m -> MPa

   soilWaterPotential = smp_mpa

   end function soilWaterPotential

end module clm_netcdf_tools
