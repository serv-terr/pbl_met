! Psychrometry - Fortran module, containing some useful routines related to temperature,
!                relative humidity and related quantities.
!
! This Fortran module is part of the pbl_met library, released as open-source under the LGPL V3.0 license.
!
! Written by: Mauri Favaron (Servizi Territorio srl)
!
MODULE Psychrometry

	IMPLICIT NONE
	
	PRIVATE
	
	! Public interface
	PUBLIC	:: WaterSaturationPressure	! Function - Saturation vapor pressure at a given temperature
	PUBLIC	:: WaterVaporPressure		! Function - Water vapor partial pressure
	PUBLIC	:: RelativeHumidity		! Function - Relative humidity
	PUBLIC	:: DewPointTemperature		! Function - Approximate dew point temperature
	PUBLIC	:: WetBulbTemperature		! Function - Wet bulb temperature estimate, given dry bulb temperature, relative humidity and pressure
	PUBLIC	:: SonicTemperature		! Function - Estimate ultrasonic temperature given dry bulb temperature, relative humidity and pressure

CONTAINS

	! Water vapor saturation pressure, given temperature
	FUNCTION WaterSaturationPressure(Ta) RESULT(es)
	
		! Routine arguments
		REAL, INTENT(IN)	:: Ta	! Air temperature (K)
		REAL			:: es	! Saturation vapor pressure (hPa)
		
		! Locals
		! -none-
		
		! Compute water saturation pressure according to the basic definition
		IF(Ta > 273.15) THEN
			es = EXP(-6763.6/Ta - 4.9283*LOG(Ta) + 54.23)
		ELSE
			es = EXP(-6141.0/Ta + 24.3)
		END IF
		
	END FUNCTION WaterSaturationPressure
	
	
	! Water vapor partial pressure, given wet and dry bulb temperatures and
	! air pressure.
	!
	FUNCTION WaterVaporPressure(Tw, Td, Pa) RESULT(Ew)
	
		! Routine arguments
		REAL, INTENT(IN)	:: Tw	! Wet bulb temperature (K)
		REAL, INTENT(IN)	:: Td	! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Pa	! Atmospheric pressure (hPa)
		REAL				:: Ew	! Water vapor partial pressure (hPa)
		
		! Locals
		REAL	:: TwetCelsius
		REAL	:: ExcessTemp
		REAL	:: FractionalDeltaP
		
		! Compute the information desired
		TwetCelsius = Tw - 273.15
		ExcessTemp  = Td - Tw		! In Nature dry bulb temperature is greater or equal to wet bulb temperature
		IF(ExcessTemp > 0.) THEN
			FractionalDeltaP = (0.00066/10.) * (1. + 0.00115*TwetCelsius)*ExcessTemp
			Ew               = WaterSaturationPressure(Tw) - FractionalDeltaP * Pa
		ELSE
			Ew               = -9999.9
		END IF

	END FUNCTION WaterVaporPressure
	
	
	! Relative humidity, given wet and dry bulb temperatures and
	! air pressure.
	!
	FUNCTION RelativeHumidity(Tw, Td, Pa) RESULT(RelH)
	
		! Routine arguments
		REAL, INTENT(IN)	:: Tw	! Wet bulb temperature (K)
		REAL, INTENT(IN)	:: Td	! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Pa	! Atmospheric pressure (hPa)
		REAL			:: RelH	! Relative humidity (%)
		
		! Locals
		REAL	:: RelH
		
		! Compute the information desired
		RelH = 100. * WaterVaporPressure(Tw, Td, Pa) / WaterSaturationPressure(Td)

	END FUNCTION RelativeHumidity
	
	
	! Estimate wet bulb temperature from dry bulb temperature, relative
	! humidity and pressure.
	!
	! The estimation is computed by solving the equation
	!
	!	Delta(Tw, Td, Ur, Pa) = 0
	!
	! for "Tw", where "Delta" is found later in the auxiliary functions
	! part of this module. As "Delta" is not everywhere differentiable with
	! respect to "Tw", for prudence a derivative-independent solver is used.
	! Actually, a two-stage approach has been followed: in the first stage
	! an initial rough bracketing of the solution is progressively made
	! smaller by bisection method. In second stage, the final solution is
	! found by secant method.
	!
	! Usage note:	Rough and fine tolerances, "RoughTol" and "FineTol", are
	! ===========	typically set to 0.1 and 0.001 respectively. In my feeling
	!		there is no real need to change them, so I made both parameters
	! optional with appropriate defaults. But on occasions you may want to experiment
	! with different values. In this case, you should ensure that
	!
	!	RoughTol << FineTol
	!
	! I recommend the fine tolerance to be some orders of magnitude smaller than
	! the rough tolerance; the smaller the rough tolerance, the higher iteration count
	! will be in bisection phase (which is more robust than secant method, but less
	! "efficient", in the sense convergence is slower).
	!
	FUNCTION WetBulbTemperature(Td, Ur, Pa, RoughTol, FineTol, MaxIter, Method) RESULT(Tw)
	
		! Routine arguments
		REAL, INTENT(IN)		:: Td		! Dry bulb (that is "ordinary") temperature (K)
		REAL, INTENT(IN)		:: Ur		! Relative humidity (%)
		REAL, INTENT(IN)		:: Pa		! Air pressure (hPa)
		REAL, INTENT(IN), OPTIONAL	:: RoughTol	! Maximum bracketing step error admitted on wet bulb temperature (K, default 0.1)
		REAL, INTENT(IN), OPTIONAL	:: FineTol	! Maximum refinement step error admitted on wet bulb temperature (K, default 0.001)
		INTEGER, INTENT(IN), OPTIONAL	:: MaxIter	! Maximum number of iterations (default: 100)
		INTEGER, INTENT(IN), OPTIONAL	:: Method	! Method used for performing calculations (1:Standard (default), 2:Simplified - see R. Stull, "Wet bulb temperature from relative humidity and air temperature", Bulletin of the AMS, Nov 2011)
		REAL				:: Tw		! Wet bulb temperature (K)
		
		! Locals
		REAL	:: rRoughTol
		REAL	:: rFineTol
		INTEGER	:: iMaxIter
		INTEGER	:: iMethod
		REAL	:: a, b			! Minimum and maximum of bracketing interval
		REAL	:: da, db		! Delta values corresponding to a and b respectively
		
		! Set default input parameters
		IF(PRESENT(RoughTol)) THEN
			rRoughTol = RoughTol
		ELSE
			rRoughTol = 0.1
		END IF
		IF(PRESENT(FineTol)) THEN
			rFineTol = FineTol
		ELSE
			rFineTol = 0.001
		END IF
		IF(PRESENT(MaxIter)) THEN
			iMaxIter = MaxIter
		ELSE
			iMaxIter = 100
		END IF
		IF(PRESENT(Method)) THEN
			iMethod = Method
		ELSE
			iMethod = 1
		END IF
		
		! Dispatch execution based on method
		SELECT CASE(iMethod)
		
		CASE(1)
		
			! Bracket solution using bisection method first
			CALL Bisect(0., Td, Ur, Pa, rRoughTol, a, b, da, db)
			Tw = Secant(a, b, da, db, Td, Ur, Pa, rFineTol, iMaxIter)
			
		CASE(2)
		
			! Stull simplified method
			Tw = (Td-273.15) * ATAN(0.151977*SQRT(Ur + 8.313659)) + ATAN(Td-273.15 + Ur) - ATAN(Ur - 1.676331) + &
				 0.00391838*Ur**1.5 * ATAN(0.023101 * Ur) - 4.686035 + 273.15
		
		CASE DEFAULT
		
			! Bracket solution using bisection method first
			CALL Bisect(0., Td, Ur, Pa, rRoughTol, a, b, da, db)
			Tw = Secant(a, b, da, db, Td, Ur, Pa, rFineTol, iMaxIter)
			
		END SELECT
		
	END FUNCTION WetBulbTemperature
	!
	! Motivations and whys - I've chosen a two-staged approach in which first is
	! ====================   bisection because this algorithm is sturdy, although
	!                        inefficient. As "Delta" is a monotonically increasing
	! function, but with one essential discontinuity at 0 °C (just where we need it
	! the most) I preferred this approach to bracket the solution to a tiny interval
	! so that the chance of finding adverse effects due to the discontinuity are
	! minimized. Once the search interval is well reduced
	! the final solution is found by secant method, more efficient but
	! somewhat less robust than bisection.
	!
	! That "Delta" is really increasing with "Tw" you can check on yourself by
	! direct inspection or testing (I've used both). Anyway, monotonicity of
	! "Delta" is essential for this routine to work as intended.
	!
	! Note about Stull method. As you can see I've implemented Stull's new simplified
	! method (non-default parameter Method==2). Then I've tested it, and found it to
	! depart quite significantly from the true value; on occasions I've noticed the
	! predicted wet bulb temperature to exceed dry bulb, which cannot be for physical reasons.
	! Investigations should be performed to check where is Stull method best suited. I guess
	! the range will depend on pressure being close to reference value.
	
	
	! Estimate dew point temperature using Magnus formula enhanced using Arden Buck equation
	FUNCTION DewPointTemperature(Td, Ur) RESULT(Dp)
	
		! Routine arguments
		REAL, INTENT(IN)				:: Td		! Dry bulb (that is "ordinary") temperature (K)
		REAL, INTENT(IN)				:: Ur		! Relative humidity (%)
		REAL						:: Dp
		
		! Locals
		REAL, PARAMETER	:: a =   6.112
		REAL, PARAMETER	:: b =  17.62
		REAL, PARAMETER	:: c = 243.12
		REAL, PARAMETER	:: d = 234.5
		REAL		:: T, G
		
		! Convert temperature to °C (all relations we use assume Celsius degrees)
		! and then obtain dew point temperature
		T  = Td - 273.15
		G  = LOG(Ur/100.0*EXP((b-T/d)*(T/(c+T))))
		Dp = c*G/(b-G) + 273.15
		
	END FUNCTION DewPointTemperature
	

	! Estimate sonic temperature given dry bulb ("normal") temperature, relative
	! humidity and atmospheric pressure.
	!
	! Routine "SonicTemperature" must compute wet bulb temperature estimate prior
	! to compute the desired sonic temperature value. The most apparent consequence
	! is tolerances and method are necessary too. The second effect is the resulting
	! estimate, based itself on estimates, may be quite poor.
	!
	! See documentation of "WetBulbTemperature" for clarifications.
	!
	FUNCTION SonicTemperature(Td, Ur, Pa, RoughTol, FineTol, MaxIter, Method) RESULT(Ts)
	
		! Routine arguments
		REAL, INTENT(IN)		:: Td		! Dry bulb (that is "ordinary") temperature (K)
		REAL, INTENT(IN)		:: Ur		! Relative humidity (%)
		REAL, INTENT(IN)		:: Pa		! Air pressure (hPa)
		REAL, INTENT(IN), OPTIONAL	:: RoughTol	! Maximum bracketing step error admitted on wet bulb temperature (K, default 0.1)
		REAL, INTENT(IN), OPTIONAL	:: FineTol	! Maximum refinement step error admitted on wet bulb temperature (K, default 0.001)
		INTEGER, INTENT(IN), OPTIONAL	:: MaxIter	! Maximum number of iterations (default: 100)
		INTEGER, INTENT(IN), OPTIONAL	:: Method	! Method used for performing calculations (1:Standard (default), 2:Simplified - see R. Stull, "Wet bulb temperature from relative humidity and air temperature", Bulletin of the AMS, Nov 2011)
		REAL				:: Ts		! Sonic temperature (K)
		
		! Locals
		REAL	:: rRoughTol
		REAL	:: rFineTol
		INTEGER	:: iMaxIter
		INTEGER	:: iMethod
		REAL	:: Tw
		
		! Set default input parameters
		IF(PRESENT(RoughTol)) THEN
			rRoughTol = RoughTol
		ELSE
			rRoughTol = 0.1
		END IF
		IF(PRESENT(FineTol)) THEN
			rFineTol = FineTol
		ELSE
			rFineTol = 0.001
		END IF
		IF(PRESENT(MaxIter)) THEN
			iMaxIter = MaxIter
		ELSE
			iMaxIter = 100
		END IF
		IF(PRESENT(Method)) THEN
			iMethod = Method
		ELSE
			iMethod = 1
		END IF
		
		! Compute the ultrasonic anemometer temperature estimate by
		! applying the direct definition
		Tw = WetBulbTemperature(Td, Ur, Pa, RoughTol, FineTol, MaxIter, Method)
		Ts = Td*(1.+0.51*0.622*WaterVaporPressure(Tw, Td, Pa)/Pa)
		
	END FUNCTION SonicTemperature
	

	! ***************************************
	! * Auxiliary functions (not accessible *
	! * through public interface)           *
	! ***************************************
	

	! Auxiliary function used by "TWET" for estimating wet bulb temperature. Given dry
	! bulb temperature, relative humidity and air pressure the wet bulb temperature is
	! the value of "Tw" at which the auxiliary function is 0.
	!
	! A simple analysis may show the auxiliary function to be monotonically increasing
	! with Tw in the interval 0 <= Tw <= Td.
	!
	! It is useful to understand where "Delta" comes from. The starting point is the
	! equation giving water vapor partial pressure,
	!
	!	E = ESAT(Tw) - 0.00066*(1.+0.00115*(Tw-273.15))*(Pa/10.)*(Td-Tw)				       (1)
	!
	! where "Tw" and "Td" are wet and dry bulb temperatures, "Pa" is air pressure,
	! and ESAT(T) the water vapor saturation pressure at temperature T.
	!
	! Now, let's consider water vapor partial pressure: the following definition
	! connects it to relative humidity, "Ur", and ESAT(Td).
	!
	!	Ur = 100.*E/ESAT(Td)
	!
	! This relation is the same as
	!
	!	E = (Ur/100.)*ESAT(Td)
	!
	! which, upon replacing in formula (1) yields
	!
	!	(Ur/100.)*ESAT(Td) = ESAT(Tw) - 0.00066*(1.+0.00115*(Tw-273.15))*(Pa/10.)*(Td-Tw)
	!
	! or
	!
	!	ESAT(Tw) - 0.00066*(1.+0.00115*(Tw-273.15))*(Pa/10.)*(Td-Tw) - (Ur/100.)*ESAT(Td) = 0  (2)
	!
	! This latter is an equation in "Tw", whose solution is the desired wet bulb
	! temperature. Monotonicity with respect to "Tw" then guarantees the solution
	! uniqueness (not its existence, however: ESAT(T) has a discontinuity at 0 °C
	! at which existence cannot be ensured a priori).
	!
	! The left member of equation (2) can be considered a function in "Tw", whose
	! value starts negative, to reach zero at wet bulb temperature, and finally
	! becomes positive. By solving it numerically, we get the desired wet bulb temperature.
	!
	FUNCTION Delta(Tw, Td, Ur, Pa) RESULT(d)
	
		! Routine arguments
		REAL, INTENT(IN)	:: Tw	! Tentative wet bulb temperature (K)
		REAL, INTENT(IN)	:: Td	! Known dry bulb temperature (K)
		REAL, INTENT(IN)	:: Ur	! Known relative humidity (%)
		REAL, INTENT(IN)	:: Pa	! Known atmospheric pressure (hPa)
		REAL			:: d	! The corresponding value of auxiliary function.
		
		! Locals
		! -none-
		
		! Compute the information desired
		d = WaterVaporPressure(Tw, Td, Pa) - (Ur/100.)*WaterSaturationPressure(Td)

	END FUNCTION Delta
	
	
	! Dedicated implementation of bisection method. It differs from the
	! standard algorithm by:
	!
	!	1)	It solves *only* equation "Delta() == 0" (see above)
	!	2)	No limit on iteration count
	!
	! The reason of the second point above is that the number of iterations
	! required, O(log2(273,15/Tol)), is always small if accuracy is in the
	! expected range 0.1-0.01.
	!
	! Bisection is used to restrict the initial solution bracketing interval
	! [0,Td] to [TwMin,TwMax] where
	!
	!	TwMax - TwMin <= Tol
	!
	! so that further use of secant method is guaranteed to easily converge.
	! Subroutine interface of Bisect is designed to provide all initialization
	! data (namely, including "da" and "db") to Secant saving a couple of function
	! evaluation: it *is* redundant, but this redundancy is desired.
	!
	SUBROUTINE Bisect(TdMin, TdMax, Ur, Pa, Tol, a, b, da, db)
	
		! Routine arguments
		REAL, INTENT(IN)	:: TdMin	! Initial lower bound of temperature (K)
		REAL, INTENT(IN)	:: TdMax	! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Ur		! Relative humidity (%)
		REAL, INTENT(IN)	:: Pa		! Atmospheric pressure (hPa)
		REAL, INTENT(IN)	:: Tol		! Tolerance (K)
		REAL, INTENT(OUT)	:: a		! Lower bound on temperature (K)
		REAL, INTENT(OUT)	:: b		! Upper bound on temperature (K)
		REAL, INTENT(OUT)	:: da		! Value of "Delta" at "a"
		REAL, INTENT(OUT)	:: db		! Value of "Delta" at "b"
		
		! Locals
		REAL	:: p
		REAL	:: dp
		
		! Initialize
		a  = TdMin
		da = Delta(a, TdMax, Ur, Pa)
		b  = TdMax
		db = Delta(b, TdMax, Ur, Pa)
		
		! Main loop: bisect interval until rough tolerance is met, or an error is found
		DO
			p  = a + (b-a)/2
			dp = Delta(p, TdMax, Ur, Pa)
			IF(da*dp > 0.) THEN
				a  = p
				da = dp
			ELSE
				b  = p
				db = dp
			END IF
			IF((b-a)/2. < Tol) EXIT
		END DO
		
	END SUBROUTINE Bisect
	
	
	! Dedicated routine for refining the estimate of wet bulb temperature
	! obtained from Bisect.
	FUNCTION Secant(a0, b0, da0, db0, Td, Ur, Pa, Tol, MaxIter) RESULT(Tw)
	
		! Routine arguments
		REAL, INTENT(IN)	:: a0		! Initial lower bound of wet bulb temperature (K)
		REAL, INTENT(IN)	:: b0		! Initial upper bound of wet bulb temperature (K)
		REAL, INTENT(IN)	:: da0		! Value of "Delta" at "a0"
		REAL, INTENT(IN)	:: db0		! Value of "Delta" at "b0"
		REAL, INTENT(IN)	:: Td		! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Ur		! Relative humidity (%)
		REAL, INTENT(IN)	:: Pa		! Atmospheric pressure (hPa)
		REAL, INTENT(IN)	:: Tol		! Tolerance (K)
		INTEGER, INTENT(IN)	:: MaxIter	! Maximum number of iterations
		REAL			:: Tw		! Wet bulb temperature (K)
		
		! Locals
		REAL	:: p
		REAL	:: dp
		REAL	:: a
		REAL	:: da
		REAL	:: b
		REAL	:: db
		INTEGER	:: Iteration
		
		! Initialization
		a  = a0
		da = da0
		b  = b0
		db = db0
		Iteration = 1
		
		! Main loop
		DO
			p = b - db*(b-a)/(db-da)
			IF(ABS(p - b) < Tol) EXIT
			a  = b
			da = db
			b  = p
			db = Delta(p, Td, Ur, Pa)
			Iteration = Iteration + 1
			IF(Iteration >= MaxIter) EXIT
		END DO
		
		! Transmit result and leave
		Tw = p
		
	END FUNCTION Secant

END MODULE Psychrometry

