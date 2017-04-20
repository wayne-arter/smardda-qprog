module bigobj_h

  use const_kind_m

! public types

!> type storing bigobj data
  type, public :: bigobj_t
     character(len=80) :: formula !< bigobj formula
     real(kr8) :: f !< power split (math variable name allowed)
     integer(ki4) :: nrpams !< number of real parameters
     integer(ki4) :: nipams !< number of integer parameters
     real(kr8), dimension(:), allocatable   :: rpar !< general real parameters
     integer(ki4), dimension(:), allocatable   :: npar !< general integer parameters
     real(kr8) :: pow !< power
  end type bigobj_t

end module bigobj_h
