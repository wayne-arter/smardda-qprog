module qcontrol_h

  use const_kind_m

!! public types

!> run control parameters
  type, public :: qparams_t
     character(len=80) :: control !< option control parameter
     real(kr8) :: realpar !< real control parameter
     integer(ki4) :: intpar !< integer control parameter
     logical :: logicpar !< logical control parameter
  end type qparams_t

!> file names
  type, public :: qfiles_t
     character(len=80)  :: out       !< output data
     character(len=80)  :: log           !< log file
     character(len=80)  :: bigobjdata         !< bigobj input data file
     character(len=80)  :: bigobjout         !< bigobj output data file
     character(len=80)  :: vtk   !< vtk file
     character(len=80)  :: gnu !< gnuplot file
  end type qfiles_t

!> plot output selectors
  type, public :: qplots_t
     logical  :: bigobjout !< bigobj output data selector
     logical  :: vtk   !< vtk plot selector
     logical  :: gnu !< gnuplot plot selector
  end type qplots_t

end module qcontrol_h
