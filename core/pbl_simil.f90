! pbl_simil  : Fortran module, providing support to Monin-Obukhov
!              (surface layer) similarity.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
module pbl_simil

    use pbl_base
    use pbl_wind
    use pbl_thermo

    implicit none
    
    private
    
    ! Public interface
    ! 0.Constants
    public	:: USTAR_PERMISSIVE
    public	:: USTAR_FINICKY
    public	:: MTD_BUSINGER
    public	:: MTD_VANULDEN_HOLTSLAG
    public	:: MTD_BELIJAARS_HOLTSLAG
    public	:: MTD_CARL
    public	:: MOL_H2O
    public	:: MOL_CO2
    public	:: MOL_AIR
    
    ! 1.Turbulence
    public	:: FrictionVelocity
    public	:: SensibleHeatFlux
    public	:: WindCorrelation
    ! 2.Stability, and stability-related
    public	:: wStar
    ! 3.Universal similarity functions
    public	:: psih
    public	:: psim
    ! 4.Vertical profiles
    public	:: WindProfile
    
    ! Constants (please do not change)
    
    integer, parameter	:: USTAR_PERMISSIVE = 0
    integer, parameter	:: USTAR_FINICKY    = 1
    
    integer, parameter	:: MTD_BUSINGER           = 1
    integer, parameter	:: MTD_VANULDEN_HOLTSLAG  = 2
    integer, parameter	:: MTD_BELIJAARS_HOLTSLAG = 3
    integer, parameter	:: MTD_CARL               = 4
    
    real(8), parameter	:: MOL_AIR = 28.96d0
    real(8), parameter	:: MOL_H2O = 18.0153d0
    real(8), parameter	:: MOL_CO2 = 44.0100d0
    
contains

	function FrictionVelocity(tEc, iMode, rvUstar) result(iRetCode)
	
		! Routine arguments
		type(EddyCovData), intent(in)					:: tEc
		integer, intent(in), optional					:: iMode
		real(8), dimension(:), allocatable, intent(out)	:: rvUstar
		integer											:: iRetCode
		
		! Locals
		integer							:: i
		integer							:: iErrCode
		integer							:: iModeIn
		integer							:: n
		real(8), dimension(:), allocatable	:: rvUW
		real(8), dimension(:), allocatable	:: rvVW
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not. tEc % isFull()) then
			iRetCode = 1
			return
		end if
		
		! Assign mode
		if(present(iMode)) then
			iModeIn = iMode
		else
			iModeIn = USTAR_PERMISSIVE
		end if
		
		! Reserve workspace
		iErrCode = tEc % getRotCovVel(1,3,rvUW)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		iErrCode = tEc % getRotCovVel(2,3,rvVW)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		n = size(rvUW)
		if(allocated(rvUstar)) deallocate(rvUstar)
		allocate(rvUstar(n))
		
		! Compute the friction velocity
		select case(iMode)
		case(USTAR_PERMISSIVE)
			rvUstar = (rvUW**2 + rvVW**2)**0.25d0
		case(USTAR_FINICKY)
			where(rvUW < 0.)
				rvUstar = sqrt(-rvUW)
			elsewhere
				rvUstar = NaN_8
			endwhere
		case default
			iRetCode = 2
		end select
		
	end function FrictionVelocity
    

	function SensibleHeatFlux(tEc, rvH0) result(iRetCode)
	
		! Routine arguments
		type(EddyCovData), intent(in)					:: tEc
		real(8), dimension(:), allocatable, intent(out)	:: rvH0
		integer											:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iModeIn
		integer								:: i
		integer								:: n
		real(8), dimension(:), allocatable	:: rvWT
		real(8), dimension(:), allocatable	:: rvTemp
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not. tEc % isFull()) then
			iRetCode = 1
			return
		end if
		
		! Reserve workspace
		iErrCode = tEc % getRotCovT(3,rvWT)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		iErrCode = tEc % getTemp(rvTemp)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		n = size(rvWT)
		if(allocated(rvH0)) deallocate(rvH0)
		allocate(rvH0(n))
		
		! Compute the sensible heat flux on vertical direction
		do i = 1, n
			rvH0(i) = rvWT(i) * RhoCp(real(rvTemp(i),kind=4) + 273.15)
		end do
		
	end function SensibleHeatFlux
	
	
	function WindCorrelation(tEc, raWindCorr) result(iRetCode)
	
		! Routine arguments
		type(EddyCovData), intent(in)						:: tEc
		real(8), dimension(:,:,:), allocatable, intent(out)	:: raWindCorr
		integer												:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iModeIn
		integer								:: i
		integer								:: j
		integer								:: k
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not. tEc % isFull()) then
			iRetCode = 1
			return
		end if
		
		! Reserve workspace
		iErrCode = tEc % getRotCovWind(raWindCorr)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		
		! Compute the three correlation coefficients, and leave variances unscaled
		! (so transform is reversible)
		do i = 1, size(raWindCorr, dim=1)
			do j = 1, 2
				do k = j+1, 3
					raWindCorr(i,j,k) = raWindCorr(i,j,k) / sqrt(raWindCorr(i,j,j)*raWindCorr(i,k,k))
					raWindCorr(i,k,j) = raWindCorr(i,j,k)
				end do
			end do
		end do
		
	end function WindCorrelation
	

	Function wStar(Ta,H0,zi) result(ws)
	
		! Routine arguments
		real, intent(in)	:: Ta	! Air temperature (K)
		real, intent(in)	:: H0	! Turbulent sensible heat flux (W/m2)
		real, intent(in)	:: zi	! Mixing height (mù
		real				:: ws
		
		! Locals
		real	:: rc
		
		! Constants
		real, parameter	:: G = 9.807
		
		! Check something can be made
		ws = NaN
		if(Ta <= 0. .or. zi < 0.) return
		
		! Compute the Deardoff velocity
		if(H0 < 0.) then
			ws = 0.
		else
			rc = RhoCp(Ta)
			if(.invalid.rc) return
			ws = (G * zi * H0 / (rc * Ta))**0.33333
		end if

	end function wStar
      
      
	function psih(zr, L, method) result(rPsiH)
	
		! Routine arguments
		real, intent(in)				:: zr		! Reference height (m)
		real, intent(in)				:: L		! Obukhov length (m)
		integer, intent(in), optional	:: method	! Method for stable part (MTD_BUSINGER: Businger; MTD_VANULDEN_HOLTSLAG:van Ulden-Holtslag, default; MTD_BELIJAARS_HOLTSLAG:Belijaars-Holtslag)
		real							:: rPsiH
		
		! Locals
		real	:: S	! Stability parameter at height 'zr'
		real	:: y
		real	:: exz
		real	:: c1
		real	:: c2
		real	:: c3
		integer	:: iMethod
		
		! Check something can be made
		rPsiH = NaN
		if(L /= 0.0) return
		S     = zr/L
		
		! Compute the desired quantity
		if(L < 0.) then
		
			! Convective part by Businger relation
			y = sqrt(1.-16.*S)
			rPsiH = 2.*alog((1.+y)/2.)
			
		else
		
			! Assign actual method
			if(present(method)) then
				iMethod = method
			else
				iMethod = MTD_VANULDEN_HOLTSLAG
			end if
			
			! Perform computing
			select case(iMethod)
			case(MTD_BUSINGER)
				rPsiH = -5. * S
			case(MTD_VANULDEN_HOLTSLAG)
				rPsiH = -17. * (1.-exp(-0.29*S))
			case(MTD_BELIJAARS_HOLTSLAG)
				exz = exp(-0.35*S)
				c1  = 1.+0.666667*S
				c2  = S-5./0.35
				c3  = 0.667*5./0.35
				rPsiH = -c1**1.5 - 0.667*c2*exz - c3 + 1.
			end select
		
		end if
		
	end function psih
    

	function psim(zr, L, stableMethod, convectiveMethod) result(rPsiM)
	
		! Routine arguments
		real, intent(in)				:: zr				! Reference height (m)
		real, intent(in)				:: L				! Obukhov length (m)
		integer, intent(in), optional	:: stableMethod		! Method for stable part (MTD_BUSINGER: Businger; MTD_VANULDEN_HOLTSLAG:van Ulden-Holtslag, default; MTD_BELIJAARS_HOLTSLAG:Belijaars-Holtslag)
		integer, intent(in), optional	:: convectiveMethod	! Method for convective part (MTD_BUSINGER: Businger, default; MTD_VANULDEN_HOLTSLAG:van Ulden-Holtslag; MTD_CARL:Carl)
		real							:: rPsiM
		
		! Locals
		real	:: S	! Stability parameter at height 'zr'
		real	:: x
		integer	:: iMethodStable
		integer	:: iMethodConvective
		
		! Constant
		real, parameter :: a = 0.7
		real, parameter :: b = 0.75
		real, parameter :: c = 5.
		real, parameter :: d = 0.35
		
		! Check something can be made
		rPsiM = NaN
		if(L /= 0.0) return
		S     = zr/L
		
		! Compute the desired quantity
		if(L < 0.) then ! Convective part
		
			! Assign actual method
			if(present(convectiveMethod)) then
				iMethodConvective = convectiveMethod
			else
				iMethodConvective = MTD_BUSINGER
			end if
			
			! Perform computing
			select case(iMethodConvective)
			case(MTD_BUSINGER)
				x    = (1.-16.*S)**0.25
				rPsiM = log((1.+x*x)/2.*((1.+x)/2.)**2) - 2.*atan(x) + 1.570796
			case(MTD_VANULDEN_HOLTSLAG)
				rPsiM = (1.-16.*S)**0.25 - 1.
			case(MTD_BELIJAARS_HOLTSLAG)
				x      = (1.-16.*S)**0.3333
				rPsiM = 1.5 * log(x*x+x+1.) - 1.7320508 * atan((2.*x+1)/1.7320508) + 0.165881
			end select
			
		else ! Stable
		
			! Assign actual method
			if(present(stableMethod)) then
				iMethodStable = stableMethod
			else
				iMethodStable = MTD_VANULDEN_HOLTSLAG
			end if
			
			! Perform computing
			select case(iMethodStable)
			case(MTD_BUSINGER)
				rPsiM = -5.*S
			case(MTD_VANULDEN_HOLTSLAG)
				rPsiM = -17. * (1.-exp(-0.29*S))
			case(MTD_CARL)
				rPsiM = -a*S-b*(S-c/d)*exp(-d*S)-b*c/d
			end select
		
		end if
		
	end function psim
	      

	! Reference: R.Sozzi, T.Georgiadis, M.Valentivi, "Introduzione alla turbolenza atmosferica: Concetti,
	! stime, misure", Pitagora Editrice (2002)
	!
	! Code is an extensive refactoring of R.Sozzi's WIND_PBL routine, written in 2000
	function WindProfile(iglo,z,zr,vr,dir,z0,hmix,us,hlm,u,v) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)					:: iglo	! Hemisphere (0: Southern, 1: Northern)
		real(8), dimension(:), intent(in)	:: z	! Heights above ground (m)
		real(8), intent(in)					:: zr	! Wind measurement height above ground (m)
		real(8), intent(in)					:: vr	! Wind speed (m/s)
		real(8), intent(in)					:: dir	! Wind direction (°)
		real(8), intent(in)					:: z0	! Aerodynamic roughness length (m)
		real(8), intent(in)					:: hmix	! Mixing height above ground (m)
		real(8), intent(in)					:: us	! Friction velocity (m/s)
		real(8), intent(in)					:: hlm	! Stability parameter (unitless)
		real(8), dimension(:), intent(out)	:: u	! Wind component U (m/s)
		real(8), dimension(:), intent(out)	:: v	! Wind component V (m/s)
		integer								:: iRetCode
		
		! Locals
		integer	:: nz		! Number of levels
		real(8)	:: rot		! Coriolis coefficient (Hemisphere dependent)
		integer	:: n_PBL	! Index of PBL top (0 if lower height is above Zi)
		real(8)	:: a
		real(8)	:: b
		real(8)	:: a0
		real(8)	:: b0
		real(8)	:: a1
		real(8)	:: b1
		real(8)	:: h_mu
		real(8)	:: s_mu
		real(8)	:: usk
		real(8)	:: al0
		real(8)	:: zz
		real(8)	:: ur
		real(8)	:: cor
		real(8)	:: umix
		real(8)	:: vmix
		real(8)	:: velmix
		real(8)	:: dirmix
		real(8)	:: ang
		real(8)	:: uu
		real(8)	:: vv
		real(8)	:: velp
		real(8)	:: dirpp
		integer	:: i
		
		! Constants
		real(8), parameter	:: TO_RAD = atan(1.d0)*4.d0 / 180.d0
		real(8), parameter	:: K      = 0.4d0	! von Karman constant
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check critical parameters
		nz = size(z)
		if(nz <= 0) then
			iRetCode = 1
			return
		end if
		if(size(u) /= nz .or. size(v) /= nz) then
			iRetCode = 2
			return
		end if
		if(z0 <= 0.d0) then
			iRetCode = 3
			return
		end if
		if(vr <= 0.d0) then
			iRetCode = 4
			return
		end if
		if(dir < 0.d0 .or. dir >= 360.) then
			iRetCode = 5
			return
		end if
		
		! Compute Coriolis coefficient, given the hemisphere
		rot = 1.d0
		if(iglo == 0) rot = -1.d0

		! Find index of the PBL top
		if(z(1) > hmix) then
			n_PBL = 0
		elseif(zr > hmix) then
			! Wind has been measured above Hmix: extend it vertically as it is
			u = -vr*sin(TO_RAD*dir)
			v = -vr*cos(TO_RAD*dir)
			return
		else
			n_PBL = nz
			do i = nz, 1, -1
				if(z(i) < hmix) then
					n_PBL = i
					exit
				end if
			end do
		end if
		! Post-condition: n_PBL is 0 if z(1) is above Hmix, or > 0

		! Estimate profile
		if(abs(hlm) < 1.d-3) then
		
			! Neutral
			a0 = 10.d0
			a1 = -5.5d0
			b0 =  4.0d0
			b1 = -4.5d0
			
		else
		
			! Stable or convective
			h_mu = 4000. * us * hlm
			if(h_mu > 0.) then
				s_mu = sqrt(h_mu)
			else
				s_mu = sqrt(h_mu)
			endif
			
			if(hlm.GT.0.) then
			
				! Stable
				a0 = 10.d0
				a1 = -5.5d0 +  1.7647d0 * s_mu
				b0 =  4.0d0 + 10.20d0 * s_mu
				b1 = -4.5d0 -  7.65d0 * s_mu
				
			else
		
				! Convective
				a  = 1.d0 + 1.581d0 * s_mu
				b  = 1.d0 + 0.027d0 * s_mu
				a0 = 10.0d0 / a
				a1 = -5.5d0 / a
				b0 = -34.d0 + 38.0d0 / b
				b1 =  24.d0 - 28.5d0 / b
				
			end if
			
		end if

		!	Calcolo del vento a zr e confronto col dato misurato
		usk = us/K
		al0 = log(zr/z0)
		zz  = (zr-z0)/hmix
		ur  = usk*(al0+b0*zz+b1*zz*zz)
		cor = vr/ur

		! Estimate wind velocity at wind speed
		al0    = log(hmix/z0)
		umix   = usk*(al0+b0+b1)*cor
		vmix   = usk*(a0+a1)*cor
		
		! Translate wind vector velocity at Hmix into polar form
		velmix = sqrt(umix**2+vmix**2)
		if(abs(vmix) < 1.d-5) then
			if(umix > 0.d0) then
				dirmix = dir +  90.0d0*rot
			else
				dirmix = dir + 270.0d0*rot
			end if
		else
			dirmix = dir + atan2(vmix,umix)/TO_RAD*rot
		end if
		if(dirmix <   0.d0) dirmix = dirmix + 360.d0
		if(dirmix > 360.d0) dirmix = dirmix - 360.d0

		! In case the first z level is above the mixing height, extend it vertically
		! as it has been estimated at Hmix. Otherwise, construct the profile until
		! Hmix, then extend it vertically as it is
		if(n_PBL == 0) then
		
			! Vertical extension of Hmix estimated wind
			u = -velmix*sin(TO_RAD*dirmix)
			v = -velmix*cos(TO_RAD*dirmix)
			return
			
		else
		
			! Compute a real profile within the PBL
			do i = 1, n_PBL
				zz  = (z(i)-z0)/hmix
				al0 = log(z(i)/z0)
				uu  = usk*(al0+b0*zz+b1*zz*zz)*cor
				vv  = usk*(a0*zz+a1*zz*zz)*cor
				if(abs(vv) < 1.d-5) then
					if(uu > 0.d0) then
						ang=90.d0
					else
						ang=270.d0
					end if
				else
					ang = atan2(vv,uu) / TO_RAD
				end if
				velp  = sqrt(uu**2+vv**2)
				dirpp = dir + ang*rot
				if(dirpp .LT. 0.  ) dirpp = dirpp + 360.
				if(dirpp .GT. 360.) dirpp = dirpp - 360.
				u(i) = -velp*SIN(TO_RAD*dirpp)
				v(i) = -velp*COS(TO_RAD*dirpp)
			end do
			
			! Propagate Hmix values above the PBL
			if(n_PBL < nz) then
				u(i) = -velmix*SIN(TO_RAD*dirmix)
				v(i) = -velmix*COS(TO_RAD*dirmix)
			end if
		end if

	end function WindProfile
!
end module pbl_simil
