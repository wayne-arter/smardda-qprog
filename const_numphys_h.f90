module const_numphys_h

  use const_kind_m

  implicit none
  private

  real(kr4) , parameter, public :: const_pi= 3.14159265_kr4 !< 4 byte pi
  real(kr8) , parameter, public :: const_pid = 3.14159265358979_kr8 !< 8 byte pi
  real(kr8) , parameter, public :: const_rrt2 = 1/sqrt(2._kr8) !< reciprocal root 2

!! neutron mass etc
  real(kr8) , parameter, public :: & !< MCNP values
 &const_aneut  = 1.008664967_kr8,            & !< Neutron mass in a.m.u..
 &const_avogad = 6.022043446928244e+23_kr8,     & !< Avogadro's number.
 &const_mneut  = .001_kr8*const_aneut/const_avogad,       & !< Neutron mass in kg
 &const_mprot  = .001_kr8/const_avogad,       & !< Proton mass in kg
 &const_melect  = 9.109382e-31_kr8,       & !< Electron mass in kg
 &const_charge  = 1.6021765e-19_kr8,       & !< Electron absolute charge in C
 &const_avgdn  = 1.0e-24_kr8*const_avogad/const_aneut,      & !< 1.e-24*Avogadro's number/neutron mass.
 &const_euler  = .577215664901532861_kr8,    & !< Euler constant used in elect ron transport.
 &const_fscon  = 137.0393_kr8,               & !< Inverse fine-structure const ant.
 &const_planck = 4.135732e-13_kr8,              & !< Planck constant.
 &const_rmass = 939.56563_kr8,              & !< rest mass of neutron in MeV
 &const_boltzmann = 1.3806488e-23_kr8,              & !< Boltzmann's constant
 &const_slite  = 299.7925_kr8                 !< Speed of light divided by 1e6

!! epsilons for general use
  real(kr4) , parameter, public :: const_eps = 1.e-6_kr4 !< epsilon for single precision
  real(kr8) , parameter, public :: const_epsd = 1.e-6_kr8 !< d.p. epsilon for single precision

!! particle pushing constants
  real(kr4) , parameter, public :: const_pushinf = 1.e+8_kr4 !< infinity for particle push
  real(kr4) , parameter, public :: const_pusheps = 1.e-8_kr4 !< epsilon for particle push
  real(kr8) , parameter, public :: const_epsbdry = 1.e-6_kr8 !< epsilon for boundary checking (=const_epsd)

  real(kr8) , parameter, public :: const_golden = (1+sqrt(5._kr8))/2 !< golden ratio

end module const_numphys_h