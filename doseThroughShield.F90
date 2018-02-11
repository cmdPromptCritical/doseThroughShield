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
REAL, dimension(:), allocatable :: murhodb(:,:)
INTEGER :: n
REAL flux, linInterp, murho, materialDensity

real tempC, tempF, FACTOR
integer ZERO_SHIFT
parameter (ZERO_SHIFT = 32, FACTOR = 5./9.)
! -----------------------------------------------Input
! Defines shield density (g/cm^3)
materialDensity = 7.874

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
allocate(murhodb(n, 2))

k = 1
do while (k <= n)
  read(99,*) murhodb(k, 1:2)

  k = k + 1
end do


! -----------------------------------------------Compute
print*, linInterp(2.0,3.0,4.0,2.0,6.0)
print*, murho(2.5, murhodb, n)

flux = (sourceActivity * exp(3.0))

! -----------------------------------------------Output
print*, "The corresponding Centigrade temperature is "
print*, flux, " degrees."
end

FUNCTION linInterp(x1, x2, x3, y1, y3) result(y2)
  REAL x1, x2, x3, y1, y2, y3

  y2 = (x2 - x1) * (y3 - y1) / (x3 - x1) + y1
END FUNCTION linInterp

FUNCTION murho(energy, murhodb, n) result(ans)
  INTEGER i, j, n
  REAL energy, mu, linInterp, x1, x3, y1, y3
  REAL, dimension(n, 2) :: murhodb

  ! cycles through database to arrray to check for interp values
  do while (i <= n)
    if (energy <= murhodb(i,1)) then
      x3 = murhodb(i,1)
      y3 = murhodb(i,2)
      x1 = murhodb(i-1,1)
      y1 = murhodb(i-1,2)
    end if

    i = i + 1
  end do

  ans = linInterp(x1, x2, x3, y1, y3)
END FUNCTION murho
