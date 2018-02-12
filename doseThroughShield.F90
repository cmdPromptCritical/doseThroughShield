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
INTEGER k, j
REAL, dimension(:), allocatable :: murhodb(:,:)
INTEGER :: n
REAL flux, linInterp, murho, shieldDensity, photonE, x, r, sourceActivity, doseRate

real tempC, tempF, FACTOR
integer ZERO_SHIFT
parameter (ZERO_SHIFT = 32, FACTOR = 5./9.)
! -----------------------------------------------Input
! Defines shield density (g/cm^3)
shieldDensity = 7.874

print*, "Enter the source activity, in Bq"
read*, sourceActivity

print*, "Enter the photon energy, in MeV"
read*, photonE

print*, "Enter the shield thickness, in centimeters"
read*, x

print*, "Enter the distance from the source to the target, in meters"
read*, r

! -----------------------------------------------LoadData
open (unit=99, file='massCoeff_Fe.txt', status='old', action='read')
read(99, *) n
allocate(murhodb(n, 2))

k = 1
do while (k <= n)
  read(99,*) murhodb(k, 1:2)

  k = k + 1
end do


! -----------------------------------------------Compute
print*, linInterp(1.0,5.0,10.0,20.0,40.0), "Linear interpolation"
print*, (5 - 1) * (40 - 20) / (10 - 1) + 10, "Linear interpolation, manual"
!print*, murhodb()
print*, murho(photonE, murhodb, n)

flux = (sourceActivity * exp(-1.0*murho(photonE, murhodb, n)*shieldDensity*x)) / (4.0*3.14159*r**2.0)
! dose rate in Sv/hr
doseRate = flux * photonE * 1.6022e-13 * murho(photonE, murhodb, n) * 1000 * 3600

! -----------------------------------------------Output
print*, "The corresponding Centigrade temperature is "
print*, flux, " - flux (cm^-2 * s^-1)"
print*, doseRate, " - dose rate (Sv/hr)"
end

FUNCTION linInterp(x1, x2, x3, y1, y3) result(y2)
  REAL x1, x2, x3, y1, y2, y3

  y2 = (x2 - x1) * (y3 - y1) / (x3 - x1) + y1
  return
END FUNCTION linInterp

FUNCTION murho(energy, murhodb, n) result(ans)
  INTEGER i, n
  REAL energy, mu, linInterp, x1, x3, y1, y3
  REAL, dimension(n, 2) :: murhodb

  ! cycles through database to arrray to check for interp values
  do while (i <= n)
    if (energy <= murhodb(i,1)) then
      x3 = murhodb(i,1)
      y3 = murhodb(i,2)
      x1 = murhodb(i-1,1)
      y1 = murhodb(i-1,2)
      exit
    end if

    i = i + 1
  end do

  ans = linInterp(x1, energy, x3, y1, y3)
END FUNCTION murho
