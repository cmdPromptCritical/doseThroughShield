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
INTEGER sourceActivity, photonE, x, r
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
! -----------------------------------------------Compute
flux = (sourceActivity * exp(3.0))

! -----------------------------------------------Output
print*, "The corresponding Centigrade temperature is "
print*, flux, " degrees."
end
