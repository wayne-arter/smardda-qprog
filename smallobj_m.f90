module smallobj_m

  use log_m
  use const_numphys_h
  use const_kind_m

  implicit none
  private

! public subroutines
  public :: &
  smallobj_initfile,  & !< open file
  smallobj_readcon,  & !< read data from file
  smallobj_generic,  & !< generic subroutine
  smallobj_write, &  !< write out object
  smallobj_delete, & !< delete object
  smallobj_close !< close file

!> parameters describing how to construct object
  type, public :: sonumerics_t
     character(len=80) :: formula !< bigobj formula
     real(kr8) :: f !< power split (math variable name allowed)
  end type sonumerics_t

! type which defines/instantiates the object
  type, public :: smallobj_t
     real(kr8) :: pow !< power
     type(sonumerics_t) :: n !< control  parameters
  end type smallobj_t

! private variables
  character(*), parameter :: m_name='smallobj_m' !< module name
  integer(ki4)  :: status   !< error status
  integer(ki4), save  :: ninso=5     !< control file unit number
  integer(ki4), save  :: noutso=6      !< output file unit number
  character(len=80), save :: controlfile !< control file name
  character(len=80), save :: outputfile !< output file name
  integer(ki4) :: i !< loop counter
  integer(ki4) :: j !< loop counter
  integer(ki4) :: k !< loop counter
  integer(ki4) :: l !< loop counter
  integer(ki4) :: ij !< loop counter
  integer(ki4)  :: ilog      !< for namelist dump after error

  contains
!---------------------------------------------------------------------
!> open file
subroutine smallobj_initfile(file,channel)

  !! arguments
  character(*), intent(in) :: file !< file name
  integer(ki4), intent(out),optional :: channel   !< input channel for object data structure
  !! local
  character(*), parameter :: s_name='smallobj_initfile' !< subroutine name
  logical :: unitused !< flag to test unit is available

  if (trim(file)=='null') then
     call log_error(m_name,s_name,1,log_info,'null filename ignored')
     return
  end if

  !! get file unit
  do i=99,1,-1
     inquire(i,opened=unitused)
     if(.not.unitused)then
        ninso=i
        if (present(channel)) channel=i
        exit
     end if
  end do

  !! open file
  controlfile=trim(file)
  call log_value("Control data file",trim(controlfile))
  open(unit=ninso,file=controlfile,status='OLD',iostat=status)
  if(status/=0)then
     !! error opening file
     print '("Fatal error: Unable to open control file, ",a)',controlfile
     call log_error(m_name,s_name,2,error_fatal,'Cannot open control data file')
     stop
  end if

end subroutine smallobj_initfile
!---------------------------------------------------------------------
!> read data from file
subroutine smallobj_readcon(selfn,channel)

  !! arguments
  type(sonumerics_t), intent(out) :: selfn !< type which data will be assigned to
  integer(ki4), intent(in),optional :: channel   !< input channel for object data structure

  !! local
  character(*), parameter :: s_name='smallobj_readcon' !< subroutine name
  character(len=80) :: smallobj_formula !< formula to be used
  real(kr8) :: power_split !< variable with meaningful name


  !! smallobj parameters
  namelist /smallobjparameters/ &
 &power_split, smallobj_formula

  !! set default smallobj parameters
  power_split=0.5_kr8

  smallobj_formula='unset'

  if(present(channel).AND.channel/=0) then
     !! assume unit already open and reading infile
     ninso=channel
  end if

  !!read smallobj parameters
  read(ninso,nml=smallobjparameters,iostat=status)
  if(status/=0) then
     !!dump namelist contents to logfile to assist error location
     print '("Fatal error reading smallobj parameters")'
     call log_getunit(ilog)
     write(ilog,nml=smallobjparameters)
     call log_error(m_name,s_name,1,error_fatal,'Error reading smallobj parameters')
  end if

  call lowor(smallobj_formula,1,len_trim(smallobj_formula))
  !! check for valid data

  !! store values
  selfn%formula=smallobj_formula

  selfn%f=power_split

end  subroutine smallobj_readcon
!---------------------------------------------------------------------
!> generic subroutine
subroutine smallobj_generic(self)

  !! arguments
  type(smallobj_t), intent(inout) :: self !< module object
  !! local
  character(*), parameter :: s_name='smallobj_generic' !< subroutine name


end subroutine smallobj_generic
!---------------------------------------------------------------------
!> write smallobj data
subroutine smallobj_write(self,channel)

  !! arguments
  type(smallobj_t), intent(in) :: self   !< smallobj data structure
  integer(ki4), intent(in), optional :: channel   !< output channel for smallobj data structure

  !! local
  character(*), parameter :: s_name='smallobj_write' !< subroutine name
  integer(ki4) :: iout   !< output channel for smallobj data structure

  !! sort out unit
  if(present(channel)) then
     iout=channel
  else
     iout=noutso
  end if

  write(iout,*,iostat=status) 'smallobj_formula'
  call log_write_check(m_name,s_name,18,status)
  write(iout,*,iostat=status) self%n%formula
  call log_write_check(m_name,s_name,19,status)
  write(iout,*,iostat=status) 'f'
  call log_write_check(m_name,s_name,20,status)
  write(iout,*,iostat=status) self%n%f
  call log_write_check(m_name,s_name,21,status)

end subroutine smallobj_write
!---------------------------------------------------------------------
!> delete object
subroutine smallobj_delete(self)

  !! arguments
  type(smallobj_t), intent(inout) :: self !< module object
  !! local
  character(*), parameter :: s_name='smallobj_delete' !< subroutine name

end subroutine smallobj_delete
!---------------------------------------------------------------------
!> close file
subroutine smallobj_close

  !! local
  character(*), parameter :: s_name='smallobj_close' !< subroutine name

  !! close file
  close(unit=ninso,iostat=status)
  if(status/=0)then
     !! error closing file
     print '("Fatal error: Unable to close control file, ",a)',controlfile
     call log_error(m_name,s_name,1,error_fatal,'Cannot close control data file')
     stop
  end if

end subroutine smallobj_close

end module smallobj_m
