module dynamic
   integer :: nx,ny,k
   real*8, dimension(:,:,:), allocatable :: data_k
   real*8, dimension(:), allocatable :: data_lon, data_lat
end module dynamic

module checks

 use netcdf
 implicit none

contains

  subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Error: netcdf"
    end if
  end subroutine check
end module


module clm

  implicit none

contains

  subroutine clm_write

   use netcdf
   use dynamic
   use checks
   implicit none

   integer :: i, ncid, varid

   print*, "entering clm_write"

   do i=1,k
     call check( nf90_open("/home/dommi/SimpleDAS/SimpleDAS/data/Australia.clm2.r.2014-12-31-72000.nc", NF90_NOWRITE, ncid) )
     call check( nf90_inq_varid(ncid, "H2OSOI_LIQ", varid) )
     call check( nf90_put_var(ncid, varid, data_k(:,:,i)) )
     call check( nf90_close(ncid) )
     print*, "clm_write: ensemble ",k
   enddo

   print*, "exiting clm_write"

  end subroutine clm_write


  subroutine clm_read(obstime,obsvar)

   use netcdf
   use dynamic
   use checks
   implicit none

   character(128) :: xname, yname, obsvar, obstime
   integer :: i,ierr
   integer :: ncid, varid, nx_tmp, ny_tmp

   print*, "entering clm_read"

   do i=1,k
    call check( nf90_open("/home/dommi/SimpleDAS/SimpleDAS/data/Australia.clm2.r.2014-12-31-72000.nc", NF90_NOWRITE, ncid) )
    call check( nf90_inq_varid(ncid, "H2OSOI_LIQ", varid) )

    if (i==1) then

      call check(nf90_inquire_dimension(ncid,3, xname,nx)) ! better replace with dimension name for safety
      call check(nf90_inquire_dimension(ncid,10,yname,ny)) ! ...

      allocate(data_k((ny-10),nx,k),stat=ierr) !ny-10 specific for soil moiture, top 5 layers are snow, bottom 5 layers bedrock
      if (ierr /= 0) stop "Error: data allocation failed"

      allocate(data_lat(nx),stat=ierr)
      if (ierr /= 0) stop "Error: latitude allocation failed"

      allocate(data_lon(nx),stat=ierr) !ny-10 specific for soil moiture, top 5 layers are snow, bottom 5 layers bedrock
      if (ierr /= 0) stop "Error: longitude allocation failed"

      call check( nf90_inq_varid(ncid, "cols1d_lat", varid) )  ! latitude
      call check( nf90_get_var(ncid, varid, data_lat(:)) ) ! latitude
      call check( nf90_inq_varid(ncid, "cols1d_lon", varid) )  ! longitude
      call check( nf90_get_var(ncid, varid, data_lon(:)) ) ! longitude

    end if


    call check(nf90_inquire_dimension(ncid,3, xname,nx_tmp)) ! better replace with dimension name for safety
    call check(nf90_inquire_dimension(ncid,10,yname,ny_tmp)) ! ...

    if (nx_tmp /= nx) print*, "Error: dimension mismatch between ensembles"
    if (ny_tmp /= ny) print*, "Error: dimension mismatch between ensembles"

    call check( nf90_get_var(ncid, varid, data_k(:,:,i),start =(/6,1/),count=(/15,nx/)) )  ! again, specific to reading soil moisture
    call check( nf90_close(ncid) )
    print*, "clm_read: ensemble ",k
   enddo

   print*, "exiting clm_read"

  end subroutine clm_read


  subroutine clm_init(clmexe)

    implicit none

    character(128), intent(in) :: clmexe
    character(256) :: cmdmsg
    integer :: cmdstat
    ! prepare clm model (namelist etc.)
    call EXECUTE_COMMAND_LINE(clmexe, cmdstat=cmdstat, cmdmsg=cmdmsg )


    if (cmdstat .ne. 0) then
     if (cmdstat .eq. -1) then
      print *, "Error: ĈLM not supported on this system"
      stop
     end if

     print *, "Error: CLM not executed"
     print *, cmdmsg
     stop
    end if

  end subroutine clm_init


  subroutine read_obslist(list_obs,n_obs)

    character(128), intent(in) :: list_obs
    integer, intent(out) :: n_obs

    ! begin read observation file
    n_obs = 0 ! number of lines
    open (unit = 7, file = list_obs)

    do
      read (7,*,end=10)
      n_obs = n_obs + 1
    enddo

    10 close (7)
    ! end read observation file

  end subroutine read_obslist


  subroutine read_obsfile
  ! read observation
  end subroutine read_obsfile


  subroutine obs_operator
   ! observation operators, including unit conversion, e.g. abs sm to relative soil moisture
  end subroutine obs_operator


end module
