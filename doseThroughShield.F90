program doseThroughShield
implicit none
#if 0
Input:
sourceActivity (integer),
photonE (int),
x (shield thickness, m),
r (distance from source to target, m)
#endif
! -----------------------------------------------Declare
INTEGER sourceActivity, photonE, x, r, k
REAL, dimension(:), allocatable :: mu(:,:)
INTEGER :: n
REAL flux

real tempC, tempF, FACTOR
integer ZERO_SHIFT
parameter (ZERO_SHIFT = 32, FACTOR = 5./9.)
! -----------------------------------------------Input
print*, "Enter the source activity, in Bq"
read*, sourceActivity

print*, "Enter the photon energy, in MeV"
read*, photonE

print*, "Enter the shield thickness, in meters"
read*, x

print*, "Enter the distance from the source to the target, in meters"
read*, r

! -----------------------------------------------LoadData
open (unit=99, file='massCoeff_Fe.txt', status='old', action='read')
read(99, *) n
allocate(mu(n, 2))

k = 1
do while (k <= n)
  read(99,*) mu(k, 1:2)

  k = k + 1
end do


! -----------------------------------------------Compute
flux = (sourceActivity * exp(3.0))

print*, mu(sourceActivity,photonE)
! -----------------------------------------------Output
print*, "The corresponding Centigrade temperature is "
print*, flux, " degrees."
end
