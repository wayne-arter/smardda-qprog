program p_qprog

  use const_kind_m
  use const_numphys_h
  use date_time_m
  use log_m
  use clock_m
  use gfile_m
  use vfile_m
  use qcontrol_h
  use qcontrol_m
  use bigobj_h
  use bigobj_m

  implicit none

! Local variables
  character(*), parameter :: m_name='qprog' !< module name
  type(qparams_t)     :: param      !< run control parameters
  type(qfiles_t)     :: file      !< names of files
  type(qplots_t)     :: plot      !< diagnostic plot selectors
  type(bigobj_t)  :: bigobj      !< bigobj object
  type(date_time_t) :: timestamp !< timestamp of run
  character(len=80) :: fileroot !< reference name for all files output by run

  integer(ki4):: nin=0 !< unit for other data
  integer(ki4):: nplot=0 !< unit for vtk files
  integer(ki4):: nprint=0 !< unit for gnuplot files
  integer(ki4):: i !< loop variable
  integer(ki4):: j !< loop variable

  character(len=80) :: ibuf !< character workspace
!--------------------------------------------------------------------------
!! initialise timing

  call date_time_init(timestamp)
  call clock_init(40)
  call clock_start(1,'run time')
!--------------------------------------------------------------------------
!! print header

  print *, '----------------------------------------------------'
  print *, 'qprog: one-line program description '
  print *, '----------------------------------------------------'
  print '(a)', timestamp%long
!--------------------------------------------------------------------------
!! file root

!! get file root from arg
  if(command_argument_count()<1) then
!! no file root specified
     print *, 'Fatal error: no file root name specified.'
     print *, 'To run qprog type at the command line:'
     print *, '   qprog fileroot'
     stop
  else
!!get fileroot
     call get_command_argument(1,value=fileroot)
  end if

!! start log
  call log_init(fileroot,timestamp)
!--------------------------------------------------------------------------
!! read control file

  call clock_start(2,'control initialisation time')
  call qcontrol_init(fileroot)
  call qcontrol_read(file,param,bigobj%n,plot)
  call clock_stop(2)
!--------------------------------------------------------------------------
!! other data for bigobj and/or read bigobj controls directly

  call clock_start(15,'initialisation time')
  call bigobj_initfile(file%bigobjdata,nin)
  !call bigobj_readcon(bigobj%n)
  call clock_stop(15)
!--------------------------------------------------------------------------
!! do the main work

  call clock_start(21,'evaluation time')

  select case(param%control)
  case default
     call bigobj_solve(bigobj)
  end select

  call clock_stop(21)
!--------------------------------------------------------------------------
!! data diagnostics to log file - optional

  call clock_start(31,'diagnostics time')

  select case(param%control)
  case default
     call bigobj_dia(bigobj)
  end select

  call clock_stop(31)
!--------------------------------------------------------------------------
!! vtk plot file(s) - optional

  call clock_start(32,'vtk plot time')

  select case(param%control)
  case default
     if(plot%vtk) then
        call vfile_init(file%vtk,'file header',nplot)
        call bigobj_writev(bigobj,'selector',nplot)
        call vfile_close
     end if
  end select

  call clock_stop(32)
!--------------------------------------------------------------------------
!! gnuplot file(s) - optional

  call clock_start(33,'gnuplot time')

  select case(param%control)
  case default
     if(plot%gnu) then
        call gfile_init(trim(file%gnu),'file header',nprint)
        call bigobj_writeg(bigobj,'selector',nprint)
        call gfile_close
     end if
  end select

  call clock_stop(33)
!--------------------------------------------------------------------------
!! output file

  call clock_start(34,'outfile_init time')
  call bigobj_initwrite(fileroot)
  call bigobj_write(bigobj)
  call bigobj_closewrite()
  call clock_stop(34)
!--------------------------------------------------------------------------
!! cleanup and closedown

  call bigobj_delete(bigobj)

  call clock_stop(1)
  call clock_summary

  call log_close
  call clock_delete
!--------------------------------------------------------------------------

end program p_qprog
