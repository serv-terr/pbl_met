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
    public	:: TempProfile
    public	:: HorizontalWindVarProfile
    public	:: VerticalWindVarProfile
    public	:: TKEDissipationProfile
    public	:: KolmogorovConstants
    
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
		real(8), intent(in)					:: hlm	! Reciprocal of Obukhov length (1/m)
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
		if(nz > 1) then
			do i = 2, nz
				if(z(i) <= z(i-1)) then
					iRetCode = 6
					return
				end if
			end do
		end if
		
		! Compute Coriolis coefficient, given the hemisphere
		rot = 1.d0
		if(iglo == 0) rot = -1.d0

		! Find index of the PBL top
		if(minval(z) > hmix) then
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
			s_mu = sqrt(abs(h_mu))
			
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
	

	! This is a very extensive refactoring of code by prof. Roberto Sozzi
	function TempProfile(z,z0,zr,Tr,gamma,hmix,Ts,us,hm,T) result(iRetCode)

		! Routine arguments
		real(8), dimension(:), intent(in)	:: z		! Heights above ground (m)
		real(8), intent(in)					:: z0		! Aerodynamic roughness length (m)
		real(8), intent(in)					:: zr		! Temperature measurement measurement height above ground (m)
		real(8), intent(in)					:: Tr		! Measured temperature (K)
		real(8), intent(in)					:: gamma	! Temperature lapse rate (above the PBL) (K/m)
		real(8), intent(in)					:: hmix		! Mixing height above ground (m)
		real(8), intent(in)					:: Ts		! Scale temperature (K)
		real(8), intent(in)					:: us		! Friction velocity (m/s)
		real(8), intent(in)					:: hm		! Reciprocal of Obukhov length (1/m)
		real(8), dimension(:), intent(out)	:: T		! Temperature (K)
		integer								:: iRetCode
		
		! Locals
		integer	:: n
		integer	:: i
		integer	:: itop
		integer	:: nzhm
		real(8)	:: smu
		real(8)	:: psi
		real(8)	:: Cfun
		real(8)	:: AA
		real(8)	:: Trif
		real(8)	:: zbase
		real(8)	:: Tbase
		real(8)	:: zbot
		real(8)	:: ztop
		real(8)	:: Ttop
		real(8)	:: DelT
		real(8)	:: we
		real(8)	:: RiE
		real(8)	:: Coe
		real(8)	:: DelZ
		real(8)	:: gz
		
		! Constants
		real(8), parameter	:: K = 0.4
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check critical parameters
		n = size(z)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		if(n /= size(T)) then
			iRetCode = 2
			return
		end if
		if(z0 <= 0.d0) then
			iRetCode = 3
			return
		end if
		if(n > 1) then
			do i = 2, n
				if(z(i) <= z(i-1)) then
					iRetCode = 4
					return
				end if
			end do
		end if

		! Find the PBL top
		if(minval(z) > hmix) then
			nzhm = 1
		else
			nzhm = n
			do i = 2, n
				if(z(i) > hmix) then
					nzhm = i - 1
					exit
				end if
			end do
		end if

		! Assign above-PBL profile, if requested
		if(nzhm == 1) then
			t(1) = tr
			do i = 2, n
				T(i) = tr + gamma*z(i)
			end do
			return
		end if

		! Some level is within-PBL: estimate the PBL profile
		smu	= hmix*hm
		if(smu > 0.d0) then
			psi = -17.d0 * (1.d0 - exp(-0.29d0*zr*hm))			! Businger, stable
		else
			psi = 2.d0*log((1.d0+sqrt(1.d0-16.d0*zr*hm))/2.d0)	! Businger, convective
		end if
		Cfun =  C_fun(smu)
		AA   = -Ts/K * (log(hmix/zr) + psi - Cfun)
		Trif =  Tr - AA

		! Estimate temperature profile up to PBL top
		do i = 1, nzhm
			if(smu > 0.d0) then
				psi = -17.d0 * (1.d0 - exp(-0.29d0*z(i)*hm))			! Businger, stable
			else
				psi = 2.d0*log((1.d0+sqrt(1.d0-16.d0*z(i)*hm))/2.d0)	! Businger, convective
			end if
			AA   = -Ts/K * (log(hmix/z(i)) + psi - Cfun)
			T(i) =  trif  + AA
		end do

		! Temperature profile above the PBL top
		if(smu >= 0.d0) then
			! Stable
			if(n > nzhm) then
				zbase = z(nzhm)
				do i = nzhm, n
					T(i) = T(nzhm) + gamma*(z(i) - zbase)
				end do
			end if
			return
		else
			! Convective
			psi  =  2.d0*log((1.d0+sqrt(1.d0-16.d0*z0*hm))/2.d0)	! Businger, convective
			Cfun =  C_fun(smu)
			AA   = -Ts/K * (log(hmix/z0) + psi - Cfun)
			Ttop =  Tr + AA
			DelT =  Ttop-T(NZHM)
			we   =  hmix**2/(1.4*hmix-2./hm)
			we   =  (-Ts*us)/gamma/we
			RiE  = 9.81/Tr * DelT*hmix/we**2
			Coe  = 3.3/RiE**0.3333 + 0.2
			DelZ = Coe*hmix
			gz   = DelT/DelZ
			DelZ = DelZ/2.

			! Entrainment Layer Depth					
			ztop = hmix + Delz
			do i = nzhm, n
				if(z(i) > ztop) exit
			end do
			itop = i

			! Entrainment Layer Profile
			Tbase = T(NZHM)
			zbot  = z(NZHM)
			do i = nzhm + 1, itop
				T(i) = Tbase + 2.d0*gz*(z(i)-zbot)
			end do

			! Free Atmosphere Profile
			Tbase = T(itop)
			do i = itop + 1, n
				T(i) = Tbase + gz*(z(i)-ztop)						
			end do
		end if

	end function TempProfile
	
	
	! Horizontal wind components variance according to
	! Rotach, Gryning, Tassone, 1996.
	! This is a very extensive refactoring of code by prof. Roberto Sozzi
	function HorizontalWindVarProfile(z,us,ws,zi,su2,sv2) result(iRetCode)
	
		! Routine arguments
		real(8), dimension(:), intent(in)	:: z
		real(8), intent(in)					:: us
		real(8), intent(in)					:: ws
		real(8), intent(in)					:: zi
		real(8), dimension(:), intent(out)	:: su2
		real(8), dimension(:), intent(out)	:: sv2
		integer								:: iRetCode

		! Locals
		integer	:: iErrCode
		integer	:: i
		integer	:: n
		real(8)	:: us2
		real(8)	:: ws2
		real(8)	:: zz
		real(8)	:: s1
		real(8)	:: s2
		
		! Constants
		real(8), parameter	:: Sh2_min = 0.05d0
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check essential parameters
		n = size(z)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		if(size(su2) /= n .or. size(sv2) /= n) then
			iRetCode = 2
			return
		end if
		if(n > 1) then
			do i = 2, n
				if(z(i) <= z(i-1)) then
					iRetCode = 3
					return
				end if
			end do
		end if
		
		! Initialization
		us2 = us**2
		ws2 = ws**2

		! Main loop
		do i = 1, n

			! Normalize height toPBL depth
			zz = z(i)/zi
			if(zz <= 1.0d0) then
				! Within-PBL

				! Mechanical part of variance
				s1 = max((5.d0 - 4.d0*zz) * us2,0.d0)
				
				! Convective part of variance
				if(ws > 0.) then
					s2 = 0.4d0 * ws2
				else
					s2 = 0.d0
				end if
				
				! Combine mechanical and convective variances
				su2(i) = max((s1 + 1.33333*s2) , Sh2_min)
				sv2(i) = max((s1 +         s2) , Sh2_min)
				
			else
			
				su2(i) = Sh2_min
				sv2(i) = Sh2_min
				
			end if

		end do

	end function HorizontalWindVarProfile
	
	
	! Estimation of vertical wind component variance and its first derivative
	! Reference: Rotach, Gryning, Tassone (1996).
	! This is a very extensive refactoring of code by prof. Roberto Sozzi
	function VerticalWindVarProfile(z,us,ws,zi,sw2,d_sw2) result(iRetCode)
	
		! Routine arguments
		real(8), dimension(:), intent(in)	:: z
		real(8), intent(in)					:: us
		real(8), intent(in)					:: ws
		real(8), intent(in)					:: zi
		real(8), dimension(:), intent(out)	:: sw2
		real(8), dimension(:), intent(out)	:: d_sw2
		integer								:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: n
		integer	:: i
		real(8)	:: us2
		real(8)	:: ws2
		real(8)	:: es1
		real(8)	:: es2
		real(8)	:: zz
		real(8)	:: sw2m
		real(8)	:: sw2c
		real(8)	:: ezz

		! Constants
		real(8), parameter	:: Sw2_min = 0.01d0

		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check essential parameters
		n = size(z)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		if(size(sw2) /= n .or. size(d_sw2) /= n) then
			iRetCode = 2
			return
		end if
		if(n > 1) then
			do i = 2, n
				if(z(i) <= z(i-1)) then
					iRetCode = 3
					return
				end if
			end do
		end if
		
		! Initialization
		us2 = us**2
		ws2 = ws**2
		es1 = 2.d0/3.d0
		es2 = 1.d0/3.d0

		! Main loop: compute the profiles desired
		do i = 1, n
	
			! Normalize altitude to PBL height
			zz = z(i)/zi
			if(zz <= 1.d0) then
				! Within PBL
				
				! Mechanical variance portion
				sw2m = max((1.7d0 - zz) * us2, 0.d0)
				
				! Convective variance portion
				if(zz > 100.d0) then
	                ezz = 0.d0
				else
					ezz = exp(-2.d0*zz)
				end if
				if(ws > 0.d0) then
					sw2c = 1.5d0 * zz**es1 * ezz * ws2 
				else
					sw2c = 0.d0
				end if
				
				! Combine mechanical and convective variances
				sw2(i) = max(sw2m + sw2c, Sw2_min)

				! First derivative of variance
				d_sw2(i) = ((1.d0-3.d0*zz)/(zz**es2) *ezz*ws2 - us2) / zi
				
			else
				! Beyond PBL
				sw2(i)   = Sw2_min
				d_sw2(i) = 0.
			end if
			
		end do
		
	end function VerticalWindVarProfile
	
	! References: Rotach, Gryning, Tassone (1996) e Ryall e Maryon (1998)
	! This is a very extensive refactoring of code by prof. Roberto Sozzi
	function TKEDissipationProfile(z,us,ws,zi,eps) result(iRetCode)
	
		! Routine arguments
		real(8), dimension(:), intent(in)	:: z
		real(8), intent(in)					:: us
		real(8), intent(in)					:: ws
		real(8), intent(in)					:: zi
		real(8), dimension(:), intent(out)	:: eps
		integer								:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: n
		integer	:: i
		real(8)	:: us3
		real(8)	:: ws3
		real(8)	:: es1
		real(8)	:: zz
		real(8)	:: epsm
		real(8)	:: epsc

		! Constants
		real(8), parameter	:: eps_min = 1.d-4
		real(8), parameter	:: K = 0.4d0
	
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check essential parameters
		n = size(z)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		if(size(eps) /= n) then
			iRetCode = 2
			return
		end if
		if(n > 1) then
			do i = 2, n
				if(z(i) <= z(i-1)) then
					iRetCode = 3
					return
				end if
			end do
		end if

		! Initialization
		us3 = us**3
		ws3 = ws**3
		es1 = 1.d0/3.d0

		! Main loop: estimate TKE dissipation profile
		do i = 1, n

			! Normalize altitude to PBL height
			zz = z(i)/zi
			if(zz <= 1.d0) then
				! Within pbl
				epsm   = us3 / (K*z(i)) * (1.d0 - 0.8d0*zz)
				if(ws > 0.d0) then
					epsc   = (1.5d0 - 1.2d0*zz**es1) * ws3 / zi
					eps(i) = max(epsm + epsc, eps_min)
				else
					eps(i) = max(epsm, eps_min)
				end if
			else
				! Above PBL
				eps(i) = eps_min
			end if

		end do
		
	end function TKEDissipationProfile
	
	
	! Reference: Degrazia e Anfossi (1998)
	function KolmogorovConstants(ws,C0u,C0v,C0w) result(iRetCode)
	
		! Routine arguments
		real(8), intent(in)		:: ws
		real(8), intent(out)	:: C0u
		real(8), intent(out)	:: C0v
		real(8), intent(out)	:: C0w
		integer					:: iRetCode
		
		! Locals
		real(8)	:: Coe
		
		! Constants
		real(8), parameter	:: PG2K  = 0.541d0			!(2 pg k)**-2/3
		real(8), parameter	:: K     = 0.4d0
		
		! Compute estimates based on convective and stable conditions
		if(ws > 0.d0) then
			! Convective
			Coe = 8.d0*K / (1.d0*0.55d0)
			C0u = Coe * (1.96d0 * 0.5d0*PG2K)**1.5d0
			C0v = Coe * (1.96d0 * 0.66667d0*PG2K)**1.5d0
			C0w = C0v
		else
			! Stable
			Coe = 8.*K / (0.64d0 * 0.55d0)
			C0u = Coe * (2.33d0 * 0.5d0*PG2K)**1.5d0
			C0v = Coe * (1.96d0 * 0.66667d0*PG2K)**1.5d0
			C0w = C0v
		end if

	end function KolmogorovConstants

	! *************
	! * Internals *
	! *************

	! From Yamada's (1976) correlations.
	function C_fun(smu) result(Cf)
	
		! Routine arguments
		real(8), intent(in)	:: smu	! hmix/L (unitless)
		real(8)				:: Cf
		
		! Locals
		! --none--
		
		if(smu < 0.d0) then
			Cf = 12.d0-8.335d0/(1.d0-0.03106d0*smu)**0.3333d0
		else
			if(smu > 18.d0) then
				Cf = -4.32d0*sqrt(smu-11.21d0)
			else
				Cf = 3.665d0-0.829d0*smu
			end if
		end if
		
	end function C_fun

end module pbl_simil
