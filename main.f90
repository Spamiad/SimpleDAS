
program SimpleDAS

  use netcdf
  use dynamic
  use clm

  implicit none


  character(2) :: starthour, endhour
  character(8) :: startday, endday
  character(128) :: clmexe, dir_clm, dir_obs, list_obs, obsvar, obstime

  integer :: i,n_obs

  real(8) :: cpu_start, cpu_finish

  print *, "Welcome to SimpleDAS!"

  ! start / stop
  startday  = '20100101'
  starthour = '00'

  endday    = '20101231'
  endhour   = '23'

  !input files
  clmexe   = '/home/dommi/cesm/cesm.exe'
  dir_clm  = '/home/dommi/cesm/run/'
  dir_obs  = '/home/dommi/cesm/obs'
  list_obs = '/home/dommi/SimpleDAS/Simple/DAS/observation_sample.txt'


  call cpu_time(cpu_start)
  k = 1 ! ensembles

  call read_obslist(list_obs,n_obs)

  do i=1,n_obs
    call clm_read(obstime,obsvar)

    call read_obsfile

    call das_main()

    call clm_write

    ! deallocate all data prior to model run to free up memory
    deallocate(data_k)
    deallocate(data_lat)
    deallocate(data_lon)

    call clm_init(clmexe)
  enddo


end program

