C
C This is the "old" PBL_MET library, as presented at Manno in occasion of the 2nd Workshop
C on Harmonisation of Atmospheric Modelling for Regulatory Purposes.
C
C Code is written in FORTRAN 77, with a few Microsoft extensions.
C
C This "old" release is distributed under the LGPL V3.0 license.
C
C The original copyright owner, Servizi Territorio srl, wants to make clear the routines
C here contained are given with no implicit or explicit guarantee about suitability for use.
C
C As you, the reader, have arrived here, we the original PBL_MET Authors assume you have
C an interest in atmospheric physics, and then the skills to decide whether the routines
C contained here are "correct" and "useful" to your specific case.
C
C Said shorter: use, at your own risk - this code has mostly a historical interest.
C
C If you desire something less experimental, more tested, more readable and in line with
C to date's Fortran, then you may prefer have a look at the current "pbl_met" modules,
C in "base" repository directory.


c& @  ==================================================================
      Function ISTAB1(z0,hour,sun_r,sun_s,vel,sigt,icor)
c     ==================================================================
*     Pasquill classification scheme of athmospheric stability based on
*     the standard deviation of horizontal wind direction fluctuations.
*     Two corrections in stability estimation can be selected:
*
*     1) correction based on the roughness length z0 (if no information
*        about z0, null value must be selected)
*     2) EPA daytime and nighttime correction based on the wind speed.
*
*     Input data:
*               z0    = roughness length (m)
*               hour  = local hour (floating point value)
*               sun_r = sunrise time
*               sun_s = sunset time
*               vel   = wind speed (m/sec)
*               sigt  = standard deviation of horizontal wind direction
*                       (sexagesimal degree)
*               icor  = 1 if EPA wind speed correction is used
*                       0 if otherwise
*
*     Output data:
*               ISTAB1 = stability class (1-6) if no error
*                        -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c -------------------------------------------------------------------------
c     3-Apr-1994  Boundary operator modified  (M.Favaron)
c -------------------------------------------------------------------------

      REAL upper(5)
      DATA UPPER/3.8,7.5,12.5,17.5,22.5/
c     ------------------------------------------------------------------
      if(sigt. LT. 0.) then
c                      error is detected!
                       ISTAB1 = -99
                       return
      endif
c     ------------------------------------------------------------------
c     roughness length correction
      if(z0. GT. 0.) then
                     sig = sigt * (z0/0.15)**0.2
      else
                     sig = sigt
      endif
c     ------------------------------------------------------------------
c     rough estimation of athmospheric stability
      IS = 1
      do  i=1,5
          if(sig .LT. upper(i)) then
                                IS =7-i
                                exit
          endif
      enddo
c     ------------------------------------------------------------------
c     EPA daytime and nightime correction
      if(icor .EQ. 0) then
c                     no EPA correction!
                      ISTAB1 = IS
                      return
      endif

      if(vel.LT.0.) then
c                   error is detected!
                    ISTAB1 = -99
                    return
      endif

      if(hour.LE.(sun_r+1.) .OR. hour.GE.(sun_s-1.)) then
c     ---->          nightime
                     go to(1,2,3,4,5,6),is
c
  1                  if(vel.LT.2.4) then
                                    ISTAB1 = 6
                     elseif(vel.LT.3.6)  then
                                         ISTAB1 = 5
                     else
                                         ISTAB1 = 4
                     endif
                     return
c
  2                  if(vel.LT.2.4) then
                                    ISTAB1 = 6
                     elseif(vel.LT.3.0) then
                                        ISTAB1 = 5
                     else
                                        ISTAB1 = 4
                     endif
                     return
c
  3                  if(vel.LT.2.4) then
                                    ISTAB1 = 5
                     else
                                    ISTAB1 = 4
                     endif
                     return
c
  4                  ISTAB1 = 4
                     return
c
  5                  if(vel.LT.5.0) then
                                    ISTAB1 = 5
                     else
                                    ISTAB1 = 4
                     endif
                     return
c
  6                  if(vel.LT.3.0) then
                                    ISTAB1 = 6
                     elseif(vel.LT.5.0) then
                                        ISTAB1 = 5
                     else
                                        ISTAB1 = 4
                     endif
                     return
c
      else
c     ---->          daytime
                     goto (7,8,9,10,10,10),is
c
  7                  if(vel.LT.3.0) then
                                    ISTAB1 = 1
                     elseif(vel.LT.4.0) then
                                        ISTAB1 = 2
                     elseif(vel.LT.6.0) then
                                        ISTAB1 = 3
                     else
                                        ISTAB1 = 4
                     endif
                     return
c
  8                  if(vel.LT.4.0) then
                                    ISTAB1 = 2
                     elseif(vel.LT.6.0) then
                                        ISTAB1 = 3
                     else
                                        ISTAB1 = 4
                     endif
                     return
c
  9                  if(vel.LT.6.0) then
                                    ISTAB1 = 3
                     else
                                    ISTAB1 = 4
                     endif
                     return
c
 10                  ISTAB1 = 4
                     return
      endif
c
      end


c& @  ==================================================================
      Function ISTAB2 (hour,salfa,sun_r,sun_s,vel,cloud)
c     ==================================================================
*     Stability classification according to Pasquill SYN method
*
*     Input data:
*               hour   = current local hour
*               salfa  = sin of solar elevation
*               sun_r  = sunrise hour
*               sun_s  = sunset hour
*               vel    = wind speed at 10 m. (m/s).
*               cloud  = clod cover (0...1)
*
*     Output data:
*               ISTAB2 = stability class (1-6) if no error
*                        -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
c     Revised 6. 4. 1994 by M.Favaron
c     ------------------------------------------------------------------

      INTEGER ma(5,5)
      REAL vupper(4), supper(2)
C
      DATA ma/1,1,2,3,3,
     $        1,2,2,3,4,
     $        2,3,3,4,4,
     $        5,5,4,4,4,
     $        6,6,5,4,4/
      DATA vupper/2.,3.,5.,6./, supper/.33,.67/
c     ------------------------------------------------------------------
      if (vel. LT. 0. .OR. abs(salfa).GT.1. .OR. cloud.LT.0. .OR.
     %    cloud.GT.1.2) then
c                      error is detected!
                       ISTAB2 = -99
                       return
      endif
c     ------------------------------------------------------------------
      if(cloud.GT. 1.) then
                       ISTAB2 = 4
                       return
      endif
c     ------------------------------------------------------------------
      if (hour. LE.(sun_r+1.) .OR. hour .GE.(sun_s-1.)) then
c                                nigthtime
                                 jmat = 5
                                 if(cloud.GE.0.5) jmat = 4
      else
c                                daytime
c                                estimate of the insolation strength
                                 sol = (1. - .5*cloud) * salfa
                                 jmat = 1
                                 do  j=1,2
                                      if(sol .LE. supper(j)) then
                                                             jmat = 4 -j
                                                             exit
                                      endif
                                 enddo
      endif

      imat = 5
      do i=1,4
          if(vel .LE. vupper(i)) then
                                 imat = i
                                 exit
          endif
      enddo
C
      ISTAB2 = ma(imat,jmat)
C
      return
      end
c

c& @  ==================================================================
      Function ISTAB3 (grad)
c     ==================================================================
*     Pasquill classification method approved by  US. Nuclear Regulatory
*     Commision.
*
*     Input data:
*               grad   = temperature lapse rate (øC/m)
*
*     Output data:
*               ISTAB3 = stability category (1-6) if no error
*                        -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      REAL upper(6)
      DATA upper/-0.019,-0.017,-0.015,-0.005,0.015,0.04/
c     ------------------------------------------------------------------
      if(abs(grad).GT.50.0) then
                          ISTAB3 = -99
                          return
      endif

      do i=1,6
         if (grad .LE. upper(i)) then
                                        ISTAB3 = i
                                        return
         endif
      enddo
      ISTAB3 = 7
C
      return
      end
c

c& @  ==================================================================
      Function ISTAB4 (vel,grad)
c     ==================================================================
*     Pasquill classification according to Probabilistic Risk Assessment
*     Procedures Guide.
*     Input data:
*               vel    = wind speed at 10 m. (m/s).
*               grad   = temperature lapse rate (øC/m)
*     Output data:
*               ISTAB4 = stability class (1-6) if no error
*                        -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      REAL vupper(4), grupper(6)
      INTEGER na(5,6)
C
      DATA na /1,1,2,3,3,
     $         2,2,2,3,3,
     $         2,3,3,3,3,
     $         2,3,4,4,4,
     $         5,5,5,4,4,
     $         6,6,6,4,4/
      DATA grupper /-1.9,-1.7,-1.5,-.5,1.5,4./ , vupper  /2.,3.,5.,6./
c     ------------------------------------------------------------------
      if(vel .LT. 0. .OR. abs(grad).GT.50.) then
c                                           error is detected!
                                            ISTAB4 = -99
                                            return
      endif
c     ------------------------------------------------------------------
      jmat = 6
      do  j=1,6
          if ((grad*100.).LT.grupper(j))  then
                                          jmat = j
                                          exit
          endif
      enddo
c
      imat = 5
      do  i=1,4
          if (vel .LT. vupper(i))  then
                                   imat = i
                                   exit
          endif
      enddo
c
      ISTAB4 = na(imat,jmat)
c
      return
      end

c& @  ==================================================================
      Function ISTAB5(hour,sun_r,sun_s,vel,sw,icor)
c     ==================================================================
*     Pasquill classification based on the standard deviation of vertical
*     wind direction fluctuations.
*     This version includes the US-EPA correction procedure based on the
*     wind speed.
*
*     Input data:
*               hour  = local hour (floating point value)
*               sun_r = sunrise time
*               sun_s = sunset time
*               vel   = horizontal wind speed (m/s)
*               sw    = standard deviation of vertical wind speed (m/s)
*               icor  = 1 if EPA wind speed correction is used
*                       0 no correction is used.
*
*     Output data:
*               ISTAB5 = stability class (1-6) if no error
*                        -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      REAL upper(5)
      DATA UPPER/2.4,5.0,7.8,10.0,11.5/
      DATA rad/57.2958/
c     ------------------------------------------------------------------
      if(vel .LT. 0. .OR. sw .LT. 0.) then
c                                     error is detected!
                                      ISTAB5 = -99
                                      return
      endif
c     ------------------------------------------------------------------
      sigf = 0.0
      if (vel.gt.0.0) sigf = (sw/vel)*rad
      IS = 1
      do  i=1,5
          if( sigf .LT. upper(i)) then
                                  IS =7-i
                                  exit
          endif
      enddo
c     ------------------------------------------------------------------
c     EPA daytime and nightime correction
      if(icor .EQ. 0) then
c                     no EPA correction!
                      ISTAB5 = IS
                      return
      endif
C
      if(hour.GE.sun_s-1.0.OR.hour.LE.sun_r+1.0) then
c     ---->          nightime
                     go to(1,1,1,1,2,3),is
c
  1                  ISTAB5 = 4
                     return
c
  2                  if(vel.LT.5.0) then
                                    ISTAB5 = 5
                     else
                                    ISTAB5 = 4
                     endif
                     return
c
  3                  if(vel.LT.3.0) then
                                    ISTAB5 = 6
                                    return
                     endif
                     if(vel.LT.5.0) then
                                    ISTAB5 = 5
                     else
                                    ISTAB5 = 4
                     endif
                     return
c
      else
c     ---->          daytime
                     goto (4,5,6,7,7,7),is
c
  4                  if(vel.LT.3.0) then
                                    ISTAB5 = 1
                                    return
                     endif
                     if(vel.LT.4.0) then
                                    ISTAB5 = 2
                                    return
                     endif
                     if(vel.lt.6.0) then
                                    ISTAB5 = 3
                                    return
                     else
                                    ISTAB5 = 4
                     endif
                     return
c
  5                  if(vel.LT.4.0) then
                                    ISTAB5 = 2
                                    return
                     endif
                     if(vel.lt.6.0) then
                                    ISTAB5 = 3
                                    return
                     else
                                    ISTAB5 = 4
                     endif
                     return
c
  6                  if(vel.lt.6.0) then
                                    ISTAB5 = 3
                     else
                                    ISTAB5 = 4
                     endif
                     return
c
  7                  ISTAB5 = 4
                     return
      endif
c
      end
c
c& @  =================================================================
      Function ISTAB6 (hour,salfa,sun_r,sun_s,vel,cloud,hn)
c     =================================================================
*     Turner stability classification.
*     input data:
*                 hour   = current local hour
*                 salfa  = sinus of solar elevation angle (rad)
*                 sun_r  = sunrise hour
*                 sun_s  = sunset hour
*                 vel    = wind speed (m/s)
*                 cloud  = cloud cover (0...1)
*                 hn     = ceiling (m). If ceiling is not measured
*                          (at daytime hours) set hn <= 0.
*     output data:
*                 ISTAB6 = stability class (1ö6) if no error
*                          -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     -----------------------------------------------------------------
      REAL alfupper(4),vupper(8)
      INTEGER  ma(9,7), irupper(4)
c
      DATA irupper /4,5,6,7/, alfupper/15.,35.,60.,90./
      DATA vupper  /2.,4.,6.,7.,9.,10.,11.,12./
      DATA ma/7,7,5,6,5,5,5,4,4,6,6,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,
     +3,4,4,4,4,4,4,4,2,2,3,3,3,3,4,4,4,1,2,2,2,2,3,3,3,4,1,1,1,2,2,2,
     +3,3,3/
c    ------------------------------------------------------------------
      if(vel.LT.0. .OR. abs(salfa).GT.1. .OR. cloud.LT.0.
     %  OR. cloud.GT.1.1) then
c                         error is detected!
                          ISTAB6 = -99
                          return
      endif
c    ------------------------------------------------------------------
      vv = vel/0.513     !(m/s) to knots

c --> Definition of Net Radiation Index
      if(cloud.GE.1 .AND. hn .LT. 2134) then
                                        ir = 3
      elseif ((hour.LE.(sun_r+1.)) .OR. (hour.GE.(sun_s-1.))) then
c                              --> nighttime:
                                   if (cloud.LE..38) then
                                                     ir = 1
                                   else
                                                     ir = 2
                                   endif
      else
c                              --> daytime:
                                   alf = ASIN(salfa)*57.29578
                                   do i=1,4
                                        if (alf.LE.alfupper(i)) then
                                                        icn = irupper(i)
                                                        exit
                                        endif
                                   enddo

                                   if(cloud.LE.0.5) then
                                        ir = icn
                                   else
                                        if(hn .LE. 2134.) then
                                                          ir = icn - 2
                                        elseif(hn.LE.4877.) then
                                                          ir = icn - 1
                                        else
                                                          ir = icn
                                        endif
                                   endif

                                   ir=max0(ir,4)

      endif
c
      imat = 9
      do  i=1,8
          if (vv .LT. vupper(i)) then
                                 imat = i
                                 exit
          endif
      enddo
c
      ISTAB6 = ma(imat,ir)
C
      return
      end
c

c& @  =================================================================
      Function ISTAB7 (hour,sun_r,sun_s,vel,rg,rn)
c     =================================================================
*     ENEL scheme for stability classification
*     input data:
*                  hour   = current local hour
*                  sun_r  = sunrise hour
*                  sun_s  = sunset hour
*                  vel    = wind speed (m/s)
*                  rg     = global solar radiation (W/m2)
*                  rn     = net solar radiation (W/m2) If net radiation
*                           is not measured then put rn=0.
*     output data:
*                  ISTAB9 = stability (1-6) if no error
*                           -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      REAL  vuppern(4),vupperd(5),riupper(5)
      INTEGER nan1(5),nan2(5),nan3(5),nad(6,6)
c
      DATA nad/1,1,2,2,3,3,1,2,2,2,3,3,2,2,2,3,3,4,2,2,3,3,3,4,3,3,3,4,4
     +,4,4,4,4,4,4,4/
      DATA riupper/140.,270.,400.,540.,700./
      DATA nan1/4,4,4,4,4/, nan2/4,4,4,5,6/, nan3/4,4,5,6,6/
      DATA vuppern/6.,5.,3.,2./, vupperd/6.,5.,4.,3.,2./
c     ------------------------------------------------------------------
      if(vel.LT.0. .OR. rg.LT.0.) then
c                                 error is detected!
                                  ISTAB7 = -99
                                  return
      endif
c     ------------------------------------------------------------------
      if((hour .LE. (sun_r+1.)) .OR. (hour .GE. (sun_s-1.))) then
c                             --> nightime
                                  ik = 5
                                  do i=1,4
                                     if(vel.GT.vuppern(i)) then
                                                           ik = i
                                                           exit
                                     endif
                                  enddo
c
                                  if(rn.EQ.0. )  then
                                                 ISTAB7 = nan2(ik)
                                                 return
                                  endif
                                  if(rn.GE.-20.) then
                                                 ISTAB7 = nan1(ik)
                                  elseif(rn.LT.-40.) then
                                                     ISTAB7 = nan3(ik)
                                  else
                                                 ISTAB7 = nan2(ik)
                                  endif
                                  return
      else
c                             --> daytime
                                  jmat = 1
                                  do j=1,5
                                     if(rg.LT.riupper(j)) then
                                                          jmat = 7-j
                                                          exit
                                     endif
                                  enddo
                                  imat = 1
                                  do j=1,5
                                     if(vel.GE.vupperd(j)) then
                                                           imat=7-j
                                                           exit
                                      endif
                                  enddo
c
      endif
c
      ISTAB7 = nad(imat,jmat)
      return
      end

c& @  =================================================================
      Function DELTAH(TV1,TV2,P1,P2)
c     =================================================================
*     This routine computes height difference between pressure levels
*     P1 and P2 (with P2<P1) and their associated TV1 and TV2
*     virtual temperatures
*
*     Input: TV1 = virtual air temperature at P1 (Kelvin)
*            TV2 = virtual air temperature at P1 (Kelvin)
*            P1  = pressure at heigth z1 (mb)
*            P2  = pressure at heigth z2 (mb)
*
*     Output: DELTAH = thickness of stratum of air (m)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(p2.GT.p1) then
                   deltah = -9999.
                   return
      endif
c
      DELTAH = 14.636 * (TV1+TV2) * ALOG(P1/P2)
c
      return
      end
c

c&@   =================================================================
      Function EAS_1(Td,Tw,p)
c     =================================================================
*     This routine computes the existing vapor pressure in air at pres-
*     sure p (mb), dry bulb temperature Td (øK) and wet-bulb temperatu-
*     re Tb (øK)
*
*     Input: Td = dry-bulb air temperature (Kelvin)
*            Tw = wet-bulb air temperature (Kelvin)
*            p  = air pressure (mb)
*
*     Output: EAS_1 = saturation vapor pression (mb)
*                     -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(Td.LT.0. .OR. Tw.LT.0. .OR. Tw.GT.Td)  then
                                                EAS_1 = -9999.
                                                return
      endif
c
      EAS_1 = ESAT(Tw) - 0.00066*(1.+0.00115*(Tw-273.15))*p*(Td-Tw)
c
      return
      end
c

c&@   =================================================================
      Function EAS_2(Tdew)
c     =================================================================
*     This routine computes the existing vapor pressure in air at the
*     dew point Tdew (øK).
*
*     Input: Tdew = dew point temperature (Kelvin)
*
*     Output: EAS_2 = saturation vapor pression (mb)
*                     -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(Tdew.LT.0.) then
                     EAS_2 = -9999.
                     return
      endif
c
      EAS_2 = ESAT(Tdew)
c
      return
      end
c

c& @  =================================================================
      Function EREL(E,ESAT)
c     =================================================================
*     This routine computes the relative humidity (%) starting from the
*     vapor pressure and the corrisponding saturated vapor pressure.
*
*     Input : E    = Existing vapor pressure (mb)
*             ESAT = saturation vapor pression (mb)
*
*     Output: EREL = relative humidity (%)
*                    -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(e.GT.esat .OR. esat.LT.0. .OR. esat.GT.1000.) then
                                                       erel = -9999.
                                                       return
      endif
c
      EREL = 100. * E / ESAT
c
      return
      end

c& @  =================================================================
      Function ESAT(T)
c     =================================================================
*     This routine computes the saturation vapor pressure in mb at the
*     temperature T.
*
*     Input: T = air temperature (Kelvin)
*
*     Output: ESAT = saturation vapor pression (mb)
*                    -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(T.LT.0.) then
                  esat = -9999.
                  return
      endif
c
      if(T.GT.273.15) then
                      ESAT = exp(-6763.6/T-4.9283*alog(T)+54.23)
      else
                      ESAT = exp(-6141.0/T+24.3)
      endif
c
      return
      end

c& @  =================================================================
      Function HUMID(p,e)
c     =================================================================
*     This routine computes the specific humidity starting from the air
*     pressure and the vapor pressure.
*
*     Input: p = air pressure (mb)
*            e = vapor pressure (mb)
*
*     Output: HUMID = specific humidity (g-water/g-air)
*                     -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(e.LT.0. .OR. p.LT.0. .OR. e.GT.1000.) then
                                               humid = -9999.
                                               return
      endif
c
      HUMID = 0.622 * e / p
c
      return
      end

c& @  =================================================================
      Function TEMPV(T,W)
c     =================================================================
*     This routine computes the virtual temperature value from tempera-
*     ture T and mixing  ratio W.
*     If virtual temperature is given as input, it will output the virtual
*     potential temperature.
*
*     Input: T = air temperature (Kelvin)
*            W = mixing ratio
*
*     Output: TEMPV = virtual temperature (Kelvin)
*                     -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(T.LT.0.) then
                  tempv = -9999.
                  return
      endif
c
      TEMPV = T * (1.+1.609*W)/(1.+W)

      return
      end

c& @  =================================================================
      FUNCTION TPOT_1(T,P)
c     =================================================================
*     This routine computes the potential temperature starting from the
*     temperature T and the air pressure P.
*
*     Input: T  = reference air temperature (Kelvin)
*            p  = air pressure (mb)
*
*     Output: TPOT_1 = potential temperature (Kelvin)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(T.LT.0. .OR. p.LT.0.) then
                               tpot_1 = -9999.
                               return
      endif
c
      TPOT_1 = T * (1000./P)**0.286

      return
      end
c

c& @  =================================================================
      FUNCTION TPOT_2(T,z)
c     =================================================================
*     This routine computes the potential temperature starting from the
*     temperature T at heigth z (m).
*
*     Input: T  = reference air temperature (Kelvin)
*            z  = heigth (m)
*
*     Output: TPOT_2 = potential temperature (Kelvin)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(z.LT.0. .OR. T.LT.0.) then
                               tpot_2 = -9999.
                               return
      endif
c
      TPOT_2 = T + 0.0098 * z
      return
      end

c&  @  =================================================================
      Function TWET(T,u)
c     =================================================================
*     This routine computes the wet-bulb temeperature starting from
*     the dry bulb temperature and the relative humidity.
*
*     Input: T = dry bulb air temperature (Kelvin)
*            u = relative humidity (%)
*
*     Output: TWET = wet-bulb temperature (Celsius)
*                    -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(T.LT.0. .OR. u.LT.0. .OR. u.GT.100.) then
                                              twet = -9999.
                                              return

      endif
c
      tw=t
c
  1   e1 = EAS_1(t,tw,1000.)
      if(e1.eq.-9999.) then
                   twet = -9999.
                   return
      endif
      esatt=ESAT(t)
      if(esatt .eq.-9999.) then
                   twet = -9999.
                   return
      endif
      ur = e1/esatt *100.
      if(ur.gt.u) then
                  tw = tw-1.
                  goto 1
      endif
c
  2   e1 = EAS_1(t,tw,1000.)
      if(e1 .EQ.-9999.) then
                   twet = -9999.
                   return
      endif
      esatt= ESAT(t)
      if(esatt .eq.-9999.) then
                   twet = -9999.
                   return
      endif
      ur = e1/esatt*100.
      if(ur.lt.(u-u/10000.))then
                            tw = tw+0.001
                            goto 2
      endif

      TWET = tw
c
      return
      end


c& @  =================================================================
      Function  VSPEC_AIR(t,p,u)
c     =================================================================
*     This routine computes the specific volume at temperature T (Kel-
*     vin), pressure P (mb) and relative humidity u(%).
*
*     Input: P = air pressure(mb)
*            T = air temperature (Kelvin)
*            u = relative humidity (%)
*
*     Output: VSPEC_AIR = specific volume at saturation (mc/kg-dry air)
*                         -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(p.LT.0. .OR. T.LT.0. .OR. u.LT.0. .OR. u.GT.100.) then
                                                    vspec_air = -9999.
                                                    return
      endif
c
      v1 = 22.4*t/ 29.3/273.15
      v2 = vspec_sat(p,t)
c
      VSPEC_AIR = v1+ (v2-v1)*u/100.
c
      return
      end
c

c& @  =================================================================
      Function  VSPEC_SAT(T,p)
c     =================================================================
*     This routine computes the saturation specific volume at tempera-
*     ture T and pressure P.
*
*     Input: P = air pressure(mb)
*            T = air temperature (Kelvin)
*
*     Output: VSPEC_SAT = specific volume at saturation (mc/kg-dry air)
*                         -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(p.LT.0. .OR. T.LT.0.) then
                               vspec_sat = -9999.
                               return
      endif
c
      hs        = 0.622*esat(t)/(p-esat(t))
c
      VSPEC_SAT = 22.4*T/273.15*(1./29.3+1/18.*hs)
c
      return
      end
c

c& @  =================================================================
      Function WMIX(p,e)
c     =================================================================
*     This routine computes mixing ratio from absolute umidity E and
*     pressure p.
*
*     Input: p = air pressure (mb)
*            e = vapor pressure (mb)
*
*     Output: WMIX = mixing ratio (g-water/g-air)
*                    -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     _________________________________________________________________
      if(e.GT.p .OR. e.LT.0. .OR. p.LT.0.) then
                                           wmix = -9999.
                                           return
      endif
c
      WMIX= 0.622*E/(P-E)
      return
      end
c
c& @  ==================================================================
      Function ALBEDO_0(jd,land,isnow,iemisph)
c     ==================================================================
*     This routine estimates albedo coefficient relating to the Julian
*     day ground surface type, snow  cover indicator and solar elevation.
*     Warning: If solar elevation is known, better precision is obtained
*     with ALBEDO_E function.
*
*
*     Input:
*             jd    =  Julian day
*             land  =  ground tipology (1..8)
*             isnow =  snow presence(1)/snow absence(0)
*             iemisph= 0 for the North emisphere
*                    = 1 for the South emisphere
*
*     Output:
*             albedo_0 = transmitted solar radiation fraction (0..1)
*                        reflected by ground.
*                        -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
*     from:   METPRO - meteorological preprocessor to CTDM program
*             PB88-162102 (US-EPA)
*
*     land-use type                  spring  summer  autumn  winter
*     1- water (sea/lake)              0.12    0.10    0.14    0.20
*     2- deciduous forest              0.12    0.12    0.12    0.50
*     3- coniferous forest             0.12    0.12    0.12    0.35
*     4- swamp                         0.12    0.14    0.16    0.30
*     5- cultivated land               0.14    0.20    0.18    0.60
*     6- grass land                    0.18    0.18    0.20    0.60
*     7- urban                         0.14    0.16    0.18    0.35
*     8- desert shrubland              0.30    0.28    0.28    0.45
*
*     N.B. During winter: with mantle of snow, winter data will be used
*                      without mantle of snow, autumn data will be used
c     -----------------------------------------------------------------
      dimension values(8,4)
      data values/ 0.12,0.12,0.12,0.12,0.14,0.18,0.14,0.30,
     %             0.10,0.12,0.12,0.14,0.20,0.18,0.16,0.28,
     %             0.14,0.12,0.12,0.16,0.18,0.20,0.18,0.28,
     %             0.20,0.50,0.35,0.30,0.60,0.60,0.35,0.45/

      if(land.LE.0 .OR. land.GT.8 .OR. isnow.LT.0 .OR. isnow.GT.1
     %   .OR. jd.LT.1 .OR. jd.GT.366) then
                                      albedo_0 = -9999.
                                      return
      endif

c     calculation of season:
      if(iemisph.eq.0) then
c       North emisphere
        istag=3
        if( jd.ge.80 .and. jd.lt.172) istag=1
        if( jd.ge.172.and. jd.lt.264) istag=2
        if(isnow.eq.1) istag=4
      else
c       South emisphere
        istag=3
        if( jd.ge.355 .or.  jd.lt.80 ) istag=2
        if( jd.ge.264.and. jd.lt.355)  istag=1
        if(isnow.eq.1) istag=4
      endif
      albedo_0= values(land,istag)

      return
      end


C&  @  ==================================================================
      Function ALBEDO_E(jd,land,isnow,s_elev,iemisph)
C     ==================================================================
*     This routine estimates albedo coefficient relating to the Julian
*     day ground surface type, snow  cover indicator and solar elevation.
*     If solar elevation is not available, ALBEDO function is required
*
*     Input
*             jd    =  Julian day
*             land  =  tipology of ground (1..8)
*             isnow =  snow presence(1)/snow absence(0)
*             s_elev=  the sine of angle of solar elevation (computed
*                      by S_EL_ANG function.
*             iemisph= 0 for the North emisphere
*                    = 1 for the South emisphere
*     Output:
*             albedo  = transmitted solar radiation fraction (0..1)
*                       reflected by ground.
*                       -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
*     from:   METPRO - meteorological preprocessor to CTDM program
*             PB88-162102 (US-EPA)
*
c     _________________________________________________________________

      if(land.LE.0 .OR. land.GT.8 .OR. isnow.LT.0 .OR. isnow.GT.1
     %   .OR. jd.LT.1 .OR. jd.GT.366) then
                                      albedo_e = -9999.
                                      return
      endif
c     internal protection:
      s_elev_ok=s_elev
      if(s_elev.lt.0.) s_elev_ok=0.
      if(s_elev_ok .eq. 0.) then
                            albedo_e=0.
                            return
      endif

      albedo_e = albedo_0(jd,land,isnow,iemisph)

c     calculation of solar elevation in ø.
      elev = ASIN(s_elev_ok)*57.2958

c     correction of albedo when elevation is below of 30ø (MPDA procedure)
      if(elev.GE.30.) return
      c1       =  1.-albedo_e
      c2       = -0.5 * c1*c1
      albedo_e = albedo_e + c1 * exp(-0.1*elev+c2)
      return
      end

C& @  ==================================================================
      Function S_EL_ANG(dlat,dlong,fuso,hour,declin)
C     ==================================================================
*     Calculation of the sine of angle of the local solar elevation
*
*     Input:
*             dlat  =  latitude (ønorthøø of degree) (Nord >0)
*             dlong =  latitude (øwestøø  of degree) (West >0)
*             fuso  =  time zone, e.g.: Mexico=-6
*                                       Los Angeles =-8
*                                       Italy= +1
*                                       Moscow=+3
*             hour  =  day time  (local and solar)
*             declin= angle of solar declination (radians)
*
*     Output:
*             s_el_ang = sine of elevation angle
*                      = -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data rad,greco/0.0174533, 3.141593/
c
      if(abs(dlat).GT.90..OR.abs(dlong).GT.180..OR.hour.LT.0..OR.
     %   hour.GT.24. .OR. declin.EQ.-9999.) then
                                            s_el_ang = -9999.
                                            return
      endif

      rlat=dlat*rad
      rlong=dlong*rad

      s_el_ang= sin(rlat)*sin(declin)-cos(rlat)*cos(declin)*
     %          cos((greco*(hour-fuso)/12.)-rlong)
c
      return
      end


c& @  ===============================================================
      Subroutine SOLE_ITA (jd,s_rise,s_set)
c     ===============================================================
*     This routine compute the sun_set and the sun_rise.
*     This version holds only for Italy.
*
*     Input:   jd     = julian day
*
*     Output:  s_rise = sunrise (-9999. if error)
*              s_set  = sunset  (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      Integer id(48)
      Real alba(48),tram(48)
c
      data id/1,8,16,24,32,39,47,55,60,67,75,83,91,98,106,114,121,128,
     %        136,144,152,159,167,175,182,189,197,205,213,220,228,236,
     %        244,251,259,267,274,281,289,297,305,312,320,328,335,342,
     %        350,358/
      data alba/8.05,8.05,8.01,7.55,7.47,7.38,7.26,7.13,7.04,6.52,6.36,
     %          6.21,6.06,5.53,5.39,5.25,5.14,5.04,4.54,4.46,4.40,4.37,
     %          4.35,4.37,4.39,4.44,4.51,4.59,5.08,5.16,5.25,5.35,5.45,
     %          5.54,6.03,6.13,6.22,6.31,6.42,6.52,7.04,7.13,7.25,7.35,
     %          7.44,7.52,7.59,8.03/
      data tram/16.45,16.56,17.06,17.16,17.28,17.38,17.50,18.01,18.08,
     %          18.18,18.28,18.39,18.49,18.58,19.09,19.19,19.27,19.36,
     %          19.46,19.54,20.02,20.07,20.12,20.14,20.13,20.12,20.07,
     %          19.59,19.50,19.41,19.28,19.15,19.00,18.48,18.32,18.17,
     %          18.03,17.50,17.36,17.22,17.10,17.00,16.51,16.44,16.40,
     %          16.38,16.39,16.42/
c

      if(jd.LE.0. .OR. jd.GT.366.) then
                                   sun_rise = -9999.
                                   sun_set  = -9999.
                                   return
      endif

      ind = 48
      do i=1,48
         if(jd.le.id(i)) then
                         ind = i
                         exit
         endif
      enddo
c
      s_rise = alba(ind)
      s_set  = tram(ind)
c
      return
      end

C& @  ==================================================================
      Function SUN_DECL(jd)
C     ==================================================================
*     This routine returns the solar declination angle
*
*     Input:
*             jd    = julian day
*
*     Output:
*             sun_decl = solar declination angle (radians)
*                        -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data greco/3.14159265/

      if(jd.le.0.or.jd.gt.366) then
                               SUN_DECL = -9999.
                               return
      endif

      sun_decl= 0.409 * cos(2.*greco*(jd-173)/365.25)

      return
      end

c
C& @  ==================================================================
      Function SUN_NET1(c_hig, c_med, c_low,albedo, sunrad)
C     ==================================================================
*     Computes the net radiation by Stull's methode.
*
*     Input:
*             c_hig =  cloudcover high (0..1)
*             c_med =  cloudcover medium (0..1)
*             c_low =  cloudcover low (0..1)
*             albedo =  albedo (0..1)
*             sunrad= global radiation
*                      computed by  function sun_rad1(...).
*     Output:
*             sun_net1 = net radiation  (W/mq)
*                        -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
c     s1= solar constant in W/mq;  s2= solar constant in øKm/s
      data s1,s2 /1370., 1.127/

      if(c_hig.lt.0. .or. c_hig.gt.1.125 .or.
     %   c_med.lt.0. .or. c_med.gt.1.125 .or.
     %   c_low.lt.0. .or. c_low.gt.1.125) then
                                          sun_net1 = -9999.
                                          return
      endif
c     Net longwawe radiation
      rn = 0.08*s1/s2*(1.-.1*c_hig -.3*c_med -.6*c_low)
      sun_net1 = (1.-albedo)*sunrad - rn
      return
      end
c
C& @  ==================================================================
      Function SUN_NET2 (cloud,T,albedo,sunrad)
C     ==================================================================
*     This routine computes the net radiation by MPDA methode.
*
*     Input:
*             cloud   = cloudcover  (0..1)
*             T       = temperature (øKelvin)
*             albedo  = albedo    (0..1)
*             sun_rad = global radiation (W/mq)
*                       computed by  function sun_rad2(...).
*     Output:
*             sun_net2 = net radiation  (W/mq)
*                        -9999. if no error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data c1,c2,c3/5.31e-13, 60., 0.12/
      data sigma/5.67e-08/

      if(cloud.LT.0..OR.cloud.GT.1.125.OR.albedo.LT.0..OR.albedo.GT.1
     %  .OR.sunrad.LT.0..OR.abs(T).GT.400.) then
                                           sun_net2 = -9999.
                                           return
      endif

      sun_net2= ((1-albedo)*sunrad+
     $   (c1*T**6.+c2*cloud-sigma*T**4))/(1.+c3)

      return
      end

C&@   ==================================================================
      Function SUN_RAD1(c_hig, c_med, c_low, sin_psi)
C     ==================================================================
*     Computes the global radiation by Stull's methode.
*
*     Input:
*             c_hig =  cloudcover high (0..1)
*             c_med =  cloudcover medium (0..1)
*             c_low =  cloudcover low (0..1)
*             sin_psi= sine of elevation angle
*                      computed by  function s_el_ang(...).
*     Output:
*             sun_rad1 = global radiation (short wave) (W/mq)
*                        -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data s/1370./

      if(c_hig.lt.0. .or. c_hig.gt.1.125 .or.
     %   c_med.lt.0. .or. c_med.gt.1.125 .or.
     %   c_low.lt.0. .or. c_low.gt.1.125) then
                                          sun_rad1 = -9999.
                                          return
      endif

c     Net sky transmissivity
      tk = (.6+.2*sin_psi)*(1.-.4*c_hig)*(1.-.7*c_med)*(1.-.4*c_low)

      if(sin_psi.ge.0.) then
           sun_rad1 = s*tk*sin_psi
      else
           sun_rad1 = 0.
      endif

      return
      end
c
C& @  ==================================================================
      Function SUN_RAD2 (cloud,sin_psi)
C     ==================================================================
*     Computes the global radiation by MPDA methode
*     Input:
*             cloud   =  cloudcover  (0..1)
*             sin_psi = sine of elevation angle
*                      computed by  function s_el_ang(...).
*     Output:
*             sun_rad2 = global radiation  (W/mq)
*                        -9999. if error
*     Note:  Using routine set_author_sun is possible to change formulas
*            switching among these authors:
*            iaut=1 MPDA method
*            iaut=2 Haurwitz, 1945 (Default)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_sun_/ i_set_sun
      data a1,a2,b1,b2/990., -30., -0.75, 3.4/
      data aa1/1098./

      if(cloud.lt.0. .or. cloud.gt.1.125) then
                                          sun_rad2 = -9999.
                                          return
      endif
      iaut=2
      if(i_set_sun. eq .1) iaut=1


c     Internal protection:
      sinmin= -a2/a1
      if(sin_psi.ge.sinmin) then
          if(iaut.eq.1) then
              sun_rad2= (a1*sin_psi+a2)*(1.+b1*cloud**b2)
          else
          sun_rad2= (aa1*sin_psi*exp(-0.057/sin_psi))*(1.+b1*cloud**b2)
          endif
      else
                        sun_rad2=0.
      endif

      return
      end
c&    ================================================================
      Subroutine SUN_SET(dlat,dlong,fuso,jd,s_rise,s_set)
c     ================================================================
*     This routine compute the sun_set and the sun_rise.
*
*     Input:   dlat   = latitude (degree North; North>0)
*              dlong  = longitude (degree West; West>0)
*              fuso   =  time zone, e.g.: Mexico=-6
*                                         Los Angeles =-8
*                                         Italy= +1
*                                         Moscow=+3
*              jd     = julian day
*
*     Output:  s_rise = sunrise (hour) (-9999. if error)
*              s_set  = sunset  (hour) (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data rad/0.0174533/, greco/3.141593/

      if(abs(dlat).GT.90..OR.abs(dlong).GT.180..OR.jd.LT.0.
     %   .OR.jd.GT.360..OR.abs(fuso).GT.12.) then
                                             s_rise = -9999.
                                             s_set  = -9999.
                                             return
      endif
      if(ABS(dlat).GE.85) then
                          if(dlat.LT.0.) then
                                         dlat = -85.
                          else
                                         dlat =  85.
                          endif
      endif

      rlat  = dlat*rad
      rlong = dlong*rad

      a    = 0.0175*jd
      b    = 0.0330*SIN(a)
      sl   = 4.871 + a + b
      decl = ASIN(0.398 * SIN(sl))
      h    = ACOS(-TAN(rlat)*TAN(decl))
      tau1 =( h+rlong-0.043*SIN(2.*sl)+0.033*sin(0.0175*jd)+greco)/.262
      tau2 =(-h+rlong-0.043*SIN(2.*sl)+0.033*sin(0.0175*jd)+greco)/.262
      t1   = fuso + tau1
      t2   = fuso + tau2
      if(t1.LT.0.) then
                   t1 = 24.+t1
      endif
      if(t2.LT.0.) then
                   t2 = 24.+t2
      endif
      s_rise = MIN(t1,t2)
      s_set  = MAX(t1,t2)
      return
      end
c&    =================================================================
      Subroutine CLOUD_RGN (Rg,Rn,sin_psi,T,   albedo,cloud_tot)
c     =================================================================
*     This routine computes the cloud cover and albedo starting from
*     observed temperature, global and net radiation.
*     Note: Net radiation is measured by a Net radiometer
*           Global Radiation is measured by a Pyranometer (short wawe,
*           solar radiation only)
*           In nigh-time hours must be: Rg=0.   and it will result albedo=0.
*
*     Input:   Rg  = Global Radiazione by a Pyranometer
*              Rn  = Net radiation by a Net Radiometer
*              sin_psi= sin of sola elevation (see S_EL_ANG routine)
*              T    = air temperature (Kelvin)
*
*     Output:  CLOUD_TOT = total cloud cover (0..1)
*              albedo    = transmitted solar radiation fraction (0..1)
*                        reflected by ground.
*                           (-9999. if error)
*
*     A particular situation can occour when Rn is not available
*      In this case:   set input Rn = -9999.
*                      Results.......
*                       a) cloud_tot is available only in day-time
*                             (-9999. in night-time)
*                       b) albedo cannot be calculated
*                         (it results -9999. day-time and 0. night-time)
*
*    WARNING: During transitional hours (i.e. around sun_rise and
*             sun_set hours), the cloud cover estimate could often be poor.
*             Particularly in these hours the averaging time for Rg, Rn
*             and sin_psi values must be at least 1 hour.
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_sun_/ i_set_sun
      data a1,a2,b1,b2/990., -30., -0.75, 3.4/
      data aa1/1098./
      data c1,c2,c3/5.31e-13, 60., 0.12/
      data sigma/5.67e-08/
c
      cloud_tot = -9999.
      albedo    = -9999.
      if(Rg.lt. 0.)  return
      if(T .lt. 200.)  return
      if(Rn.gt. Rg )  return
      if(sin_psi .eq.-9999. )  return

      iaut=2
      if(i_set_sun. eq .1) iaut=1

      c1t6= c1*T**6 -sigma*T**4

      sinpsimin= -a2/a1

      if( sin_psi.gt. sinpsimin) then
         if(iaut.eq.1) then
            rgmax= a1*sin_psi+a2
         else
            rgmax =  aa1*sin_psi*exp(-0.057/sin_psi)
         endif
c        day-time ...........
         if( Rg .ge. rgmax) then
                            cloud_tot=0.
         else
           cloud_tot= (1./b1*(Rg/rgmax-1.))**(1./b2)
         endif
         if(rn .ne. -9999.) then
            albedo=1. - ((1.+c3)*Rn - c1t6 -c2*cloud_tot)/Rg
            if(albedo.lt.0.) albedo=0.
            if(albedo.gt.1.) albedo=1.
         else
            albedo=-9999.
         endif

      else
c        night-time
         albedo=0.
         if(rn .ne. -9999.) then
           cloud_tot= ((1.+c3)*Rn-c1T6)/c2
         else
           cloud_tot=-9999.
         endif
      endif
           if(cloud_tot.gt.1.) cloud_tot=1.
           if(cloud_tot.lt.0.) cloud_tot=0.
      return
      end

c&  @  =================================================================
      Subroutine set_author_sun(irg)
c     =================================================================
*     Calculation of Solar Radiation.
*     This routine set the internal Common to the required relationships
*     obtained by different Authors, choosing among the availables ones.
*
*     irg  :  set parameters for Global Radiation
*             0 = No changes (set Default = 1)
*             1 = Computes the global radiation by MPDA method
*             2 = Haurwitz, 1945  (reported in Kasten & Czeplak, 1979)
*
*     The given parameters are used in a labeled COMMON /set_sun_/
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_sun_/ i_set_sun
      i_set_sun=irg
      return
      end

c&     ==================================================================
      Function BOWEN_R(z1,z2,T1,T2,u1,u2,p)
c     ==================================================================
*     This routine returns the value of the Bowen ratio starting from
*     two temperature and humidity measures.
*
*     Input data:
*              z1    = heigth where T1 and u1 are measured (m)
*              z2    = heigth where T2 and u2 are measured (m)
*              T1    = air temperature at z1 (Kelvin)
*              T2    = air temperature at z2 (Kelvin)
*              u1    = relative humidity at z1 (%)
*              u2    = relative humidity at z2 (%)
*              p     = air pressure (mb)
*
*     Output:
*             Bowen_r = Bowen ratio
*                     -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      BOWEN_R=-9999.

      if(z1.LT.0. .OR. z2.LT.0. .OR. T1.LT.0. .OR. T2.LT.0. .OR.
     %   u1.LT.0. .OR. u2.LT.0. .OR. u1.GT.100. .OR. u2.GT.100. .OR.
     %   p.LT.0.) return

      th1 = TPOT_2(T1,z1)
      th2 = TPOT_2(T2,z2)
      if(th1.eq.-9999.) return
      if(th2.eq.-9999.) return
      dth = th2 - th1
c
      q1  = Q(t1,u1,p)
      q2  = Q(t2,u2,p)
      if(q1.eq.-9999. .or. q2. eq.-9999.) return

      dq  = q2 - q1
c
      if (abs(dq).lt.1.e-09) then
                             bowen_r = -9999.
      else
                             bowen_r = 0.0004 * dth/dq
      endif
c
      return
      end
c
c     ==================================================================
      function Q(t,rh,p)
c     ==================================================================
      e = rh/100.*ESAT(t)
      if(e .eq.-9999.) then
                   Q   = -9999.
                   return
      endif

      q = HUMID(p,e)
      if(q.eq.-9999.) then
                   Q   = -9999.
                   return
      endif
c
      return
      end
c

c& @  =================================================================
      Function D1_PSH_C(zr,hl)
c    =================================================================
*     This routine returns first derivative with respect to z/L of the
*     universal heat function in convective conditions.
*
*     Input data:
*                iaut = 1 -> Businger relation (DEFAULT)
*        NOTE! iaut must be set up using Set_Author_pbl Routine.
*
*                zr   = reference height (m)
*                hl   = inverse of Monin Obukhov length (1/m)
*
*     Output data:
*                D1_PSH_C = first derivative of universal function
*                           in convective conditions.
*                           -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set

      if(zr.LT.0. .OR. hl.GT.0.) then
                         D1_PSH_C = -9999.
                         return
      endif

      zl = zr*hl

      if(ihc_set.ne.1) then
               iaut=1
      else
               iaut=ihc_set
      endif

      go to (1),iaut

c ->  Businger relation
 1    y  = SQRT(1.-16.*zl)
      D1_PSH_C = -16./ (y*(1.+y))
      return
      end


c& @  =================================================================
      Function D1_PSH_S(zr,hl)
c     =================================================================
*     This routine returns first derivative with respect to z/L of the
*     universal heat function in stable conditions.
*
*     Input data:
*                iaut = 1 -> Businger relation
*                       2 -> van Ulden Holtslag relation (DEFAULT)
*                       3 -> Beljaars Holtslag relation
*        NOTE! iaut must be set up using Set_Author_pbl Routine.
*
*                zr   = reference height (m)
*                hl   = inverse of Monin Obukhov length (1/m)
*
*     Output data:
*                D1_PSH_S = first derivative of universal function
*                            in stable conditions.
*                           -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set
      DATA a/1./, b/0.6667/, c/5./, d/0.35/

      D1_PSH_S = -9999.
      if(zr.LT.0. .OR. hl.LT.0.) RETURN

      zl = zr*hl

      if(ihs_set.ne.1  .or.
     %   ihs_set.ne.2  .or.
     %   ihs_set.ne.3) then
               iaut=2
      else
               iaut=ihs_set
      endif

      go to (1,2,3),iaut

c  ->    Businger relation
 1       a = zl
         D1_PSH_S = -5.
         return

c  ->    van Ulden Holslag relation
 2       D1_PSH_S= -4.93 * exp(-0.29*zl)
         return

c  ->    Beljaars Holtslag relation
 3       exz = exp(-d*zl)
         c1  = SQRT(1.+0.666667*a*zl)
         D1_PSH_S= -a*c1-b*exz*(1.-d*zl+c)
         return

      end

c& @  =================================================================
      Function D1_PSM_C(zr,hl)
c     =================================================================
*     This routine returns first derivative with respect to z/L of the
*     universal momentum function in convective conditions.
*
*     Input data:
*                iaut = 1 -> Businger relation (DEFAULT)
*                       2 -> van Ulden Holtslag relation
*                       3 -> Carl relation
*        NOTE! iaut must be set up using Set_Author_pbl Routine.
*
*                zr   = reference height (m)
*                hl   = inverse of Monin Obukhov length (1/m)
*
*     Output data:
*                D1_PSM_C = first derivative of universal function
*                           in convective conditions.
*                           -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set

      D1_PSM_C = -9999.
      if(zr.LT.0. .OR. hl.GT.0.) return

      zl = zr*hl

      if(imc_set.ne.1  .or.
     %   imc_set.ne.2  .or.
     %   imc_set.ne.3) then
               iaut=1
      else
               iaut=imc_set
      endif

      go to (1,2,3),iaut

c ->     Businger relation
 1       x        = (1. - 16.*zl)**0.25
         D1_PSM_C = -16./(x*(1.+x)*(1.+x*x))
         return

c ->     van Ulden Holtslag relation
 2       x        = (1. - 16.*zl)**0.25
         D1_PSM_C = -4./(x*x*x)
         return

c ->     Carl relation
 3       x        = (1. -16.*zl)**0.33333
         D1_PSM_C = -16./(x * (x*x+x+1))
         return

      end

c& @  =================================================================
      Function D1_PSM_S(zr,hl)
c     =================================================================
*     This routine returns first derivative with respect to z/L of the
*     universal momentum function in stable conditions.
*
*     Input data:
*                iaut = 1 -> Businger relation
*                       2 -> van Ulden Holtslag relation (DEFAULT)
*                       3 -> Beljaars Holtslag relation
*        NOTE! iaut must be set up using Set_Author_pbl Routine.
*
*                zr   = reference height (m)
*                hl   = inverse of Monin Obukhov length (1/m)
*
*     Output data:
*                D1_PSM_S = first derivative of universal function
*                            in stable conditions.
*                           -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set
      DATA a/0.7/, b/0.75/, c/5./, d/0.35/

      D1_PSM_S = -9999.
      if(zr.LT.0. .OR. hl.LT.0.) return

      zl = zr*hl

      if(ims_set.ne.1  .or.
     %   ims_set.ne.2  .or.
     %   ims_set.ne.3) then
               iaut=2
      else
               iaut=ims_set
      endif

      go to (1,2,3),iaut

c  ->    Businger relation
 1       a = zl
         D1_PSM_S = -5.
         return

c  ->    van Ulden Holslag relation
 2       D1_PSM_S= -4.93 * exp(-0.29*zl)
         return

c  ->    Beljaars Holtslag relation
 3       exz = exp(-d*zl)
         D1_PSM_S= -a-b*exz*(1.-d*zl+c)
         return

      end

c&@   =================================================================
      Function H0_0(rc,T,us,hlm)
c     =================================================================
*     This routine returns the value of the turbulent sensible heat flux
*     from basic definition.
*
*     Input data:
*                rc   = density * specific heat
*                T    = mean air temperature (Kelvin)
*                us   = friction velocity (m/s)
*                hlm  = 1/Monin Obukhov length (1/m)
*
*     Output data:
*                H0_0 = sensible heat flux (W/m2) (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      DATA hk/0.4/, g/9.81/

      if(rc.LT.0. .OR. T.LT.0. .OR. us.LT.0.) then
                                              h0_0 = -9999.
                                              return
      endif

      H0_0 = -rc * hlm * (T*us**3)/(hk*g)

      return
      end

c&@   ==================================================================
      Function H0_1 (z0,br,zt,T,zu,vel,um,rn)
C     ==================================================================
*     This routine computes the sensible heat flux according to Penman-
*     Monteith's model.
*
*     Input:
*             z0  = roughness length (m)
*             br  = typical Bowen ratio
*             T   = temperature (Kelvin) measured at zt.
*             zt  = heigth where the temperature is measured (m)
*             vel = wind velocity (m/s)
*             zu  = heigth where wind speed is measured (m)
*             um  = relative  humidity (%)
*             rn  = net radiation (W/mq)
*
*     Output:
*             H0_1 = sensible heat flux at ground (W/mq)
*                    -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data gamma/0.0004/, pres/1013./
c
      if(zt.LT.0. .OR. zu.LT.0. .OR. z0.LT.0. OR. T.LT.0. .OR.
     %   vel.LT.0. .OR. um.LT.0. .OR. um.GT.100.) then
                                                  H0_1 = -9999.
                                                  return
      endif

      rocp = RHOCP(T)

      temp = T - 273.15
      gs   = exp((6.42 - temp)/17.78)
c
      ra = alog(zu/z0)
      ra = 4.72*ra*ra/(1.+0.54*vel)
c
      de = 0.622*(1.-um/100.)*6.1078*10.**(7.5*temp/(237.3+temp))/pres
c
      rg = H_ground(T,zt,vel,zu,z0,rn)
      if(rg.eq.-9999. ) then
                   H0_1 = -9999.
                   return
      endif
c     --------------------------------------
c     if(br.GT.-2. .AND. br.LT.0.) br = -2.
c     --------------------------------------

      s  = gamma/gs
      rc = (1./gs*br-1.)*ra+rocp*de*(1.+br)/(gamma*(rn-rg))
c
      h1 = gamma*(1.+rc/ra)*(rn-rg)
      h2 = rocp*de/ra
      h3 = s+gamma*(1.+rc/ra)
c
      H0_1 = (h1-h2)/h3
      if(rn.le.0. .and. H0_1.gt.0.) H0_1 = 0.
c
      return
      end

c& @  ==================================================================
      Function H0_2(br,T,rn)
c     ==================================================================
*     Computes the sensible heat flux according to Penman-Monteith's
*     model. Modified by Holtslag.
*     (warning: better results in daytime)
*
*     Input:
*             br  = typical Bowen ratio
*             T   = air temperature at ground level (øK)
*             rn  = net radiation (W/mq)
*
*     Output:
*             H0_2 = sensitive heat flux at ground (W/mq)
*                    -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      DATA beta/20./

      if(T.LT.0.) then
                  H0_2 = -9999.
                  return
      endif

      if(rn.le.0.) then
                   s = 0.5
      else
                   s = 0.1
      endif
c
      if(br.GT.-2. .AND. br.LT.0.) br = -2.
      g = exp((6.42-(T-273.15))/17.78)
      a = (1.+g)*(0.9*rn)/((1.+br)*(0.9*rn+beta*(1.+g)))
c
      H0_2 = ((1.-a)+g)/(1.+g)*(1.-s)*rn - beta*a
c
c     if(rn.le.0. and . H0_2.gt.0.) H0_2 = 0.
c
      return
      end

c& @  ==================================================================
      Function H0_3 (z0,z1,z2,T1,T2,u1,u2,vel,zr,rn,p)
c     ==================================================================
*     This routine computes the sensible heat flux by Bowen ratio
*
*     Input:
*              z0    = roughness length (m)
*              z1    = heigth where T1 and u1 are measured (m)
*              z2    = heigth where T2 and u2 are measured (m)
*              T1    = air temperature at z1 (Kelvin)
*              T2    = air temperature at z2 (Kelvin)
*              u1    = relative humidity at z1 (%)
*              u2    = relative humidity at z2 (%)
*              vel   = wind speed (m/s)
*              zr    = height where wind speed is measured (m/s)
*              rn    = net radiation (W/mq)
*              p     = air pressure (mb)
*
*     Output:
*             h0_3 = sensible heat flux (W/mq)
*                    -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
c
      b = BOWEN_R(z1,z2,T1,T2,u1,u2,p)
      if(b.eq.-9999) then
                   H0_3 = -9999.
                   return
      endif
c
      G = H_ground(T1,z1,vel,zr,z0,rn)
      if(G.eq.-9999.) then
                   H0_3 = -9999.
                   return
      endif
c
      b1   = 1.+b
      dRn  = Rn-G

      H0L  = dRn/b1
      H0_3 = b * H0L
c
      return
      end

c&@   =================================================================
      Function HLM_0(icat,z0)
c     =================================================================
*     This routine returns the value of 1/L (L is the Monin Obukhov
*     length) starting from the stability category  and the roughness
*     length.
*     This method is based on the Gorder suggestions (1972)
*
*     Input data:
*                icat = stability category (1 .. 6)
*                z0   = roughness length (m)
*
*     Output data:
*                HLM_0 = 1/L (1/m)  (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      real a(6),b(6)

      DATA a/-0.096,-0.037,-0.002,0.,0.004,0.035/
      DATA b/0.029,0.029,0.018,0.,-0.018,-0.036/

      if(icat.LT.0 .OR. icat.GT.6 .OR. z0.LT.0.) then
                                                 HLM_0 = -9999.
                                                 return
      endif

      HLM_0 = a(icat) + b(icat)*alog10(z0)

      return
      end

c&@   =================================================================
      Function HLM_1(T,us,ts)
c     =================================================================
*     This routine return the value of 1/Monin Obukhov length starting
*     from the friction velocity and the scale temperature.
*
*     Input data:
*                T    = average air potential  temperature (Kelvin)
*                us   = friction velocity (m/s)
*                ts   = scale temperature (Kelvin)
*
*     Output data:
*                HLM_1  = 1/Monin Obukhov length (1/m)
*                         -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data hk/0.4/, g/9.81/

      if(T.LT.0. .OR. us.LT.0.)then
                               HLM_1 = -9999.
                               return
      endif

      HLM_1 = g * hk * ts/ (T * us*us)

      return
      end

c&@   ================================================================
      Function HLM_2 (rc,T,us,h0)
c     ================================================================
*     This routine returns the value of 1/Monin Obukhov length starting
*     from the friction velocity and the sensible heat flux.
*
*     Input data:
*              rc = rho * cp
*              T  = average air temperature (Kelvin)
*              us = friction velocity (m/s)
*              h0 = sensible heat flux (W/m2)
*
*     Output data:
*              HLM_2 = 1/Monin Obukhov length (1/m)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     -------------------------------------------------------------------
      data hk/0.4/, g/9.81/

      if(T.LT.0. .OR. us.LT.0.)then
                               HLM_2 = -9999.
                               return
      endif

      HLM_2 = -(g*hk*h0) / (rc*T*us*us*us)

      return
      end

c&    ================================================================
      Function HLM_3 (z0,cloud,zr,vel,T,ts)
c     ================================================================
*     This routine returns the 1/Monin Obukhov length and scale tempe-
*     rature in nightime situation.
*
*     Input data:
*              z0  = roughness length (m)
*              cloud cover (0..1)
*              zr  = reference height (m)
*              vel = wind speed at zr (m/s)
*              T   = mean air temperature (Klevin)
*
*     Output data:
*              HLM_3 = 1/Monin Obukhov length (1/m)
*                      -9999. if error
*              ts    = scale temperature (Kelvin)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     -------------------------------------------------------------------
      data hk/0.4/, c/4.7/, g/9.81/, tst/0.09/
c
      if(z0.LT.0. .OR. z0.GT.zr .OR. vel.LT.0. .OR. T.LT.0.) then
                                                             HLM_3 = -9999.
                                                             return
      endif

      alz = alog(zr/z0)
c
      ts1 = tst * (1.-0.5*CLOUD*CLOUD)

      cd  = hk/alz
      ts2 = T * cd * vel*vel / (4. * c * zr * g)

      ts  = AMIN1(ts1,ts2)

      hl0 = c * zr / alz
      hln = hk * vel*vel * T /(2.* g * ts * alz*alz)
      if(hln.GE.(2.*hl0)) then
                          HLM_3 = hln - hl0 + SQRT(hln*(hln - 2.*hl0))
                          HLM_3 = 1./HLM_3
      else
c                         otherwise L il set to 5 m
                          HLM_3 = 0.2
      endif
      return
      end

c& @  ==================================================================
      Function H_MEC(iaut,us,hlm)
c     ==================================================================
*     This routine computes the stable and neutral mechanical mixing
*     height according to several authors.
*
*     Input
*          iaut = 1 -> Nieuwstadt (1981)
*                 2 -> Blackadar&Tennekes(1968),Benkley&Schulmann
*                 3 -> Zilitinkevich (1972)
*          us   = friction velocity (m/s)
*          hlm  = 1./Monin Obukhov length (1/m)
*
*     Output
*          h_mec = mechanical mixing height  (m)
*                 -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      if( iaut.LT.0 .OR. iaut.GT.3 .OR. us.LT.0.) then
                                                  H_MEC = -9999.
                                                  return
      endif

      if(hlm.LE.0.) iaut = 2
      go to (1,2,3) iaut

c     -> Nieuwstadt
  1      H_MEC = HMEC_1(us,hlm)
         return

c     -> Blackadar&Tennekes(1968),Benkley&Schulmann
  2      H_MEC = HMEC_2(us)
         return

c     -> Zilitinkevich
  3      H_MEC = HMEC_3(us,hlm)
         return

      end

c  @  ==================================================================
      Function HMEC_1(us,hlm)
c     ==================================================================
      hmec_1 = 0.2632/hlm * (SQRT(1.+22800*us*hlm)-1.)
c
      return
      end
c
c  @  ==================================================================
      Function HMEC_2(us)
c     ==================================================================
      DATA f/1.e-04/
c
      hmec_2 = 0.2 * us/f
c
      return
      end
c
c  @  ==================================================================
      Function HMEC_3(us,hlm)
c  @  ==================================================================
      DATA f/1.e-04/, cs/0.4/

      HMEC_3 = cs * SQRT(us/(f*hlm))

      return
      end

c& @  =================================================================
      Subroutine set_author_pbl(imc,ims,ihc,ihs)
c     =================================================================
*     This routine set the internal Common to the required relationships
*     obtained by different Authors, choosing among the availables ones.
*
*     imc  :  set parameters for Momentum Flux (Convective Conditions)
*             0 = No changes
*             1 = Businger relation
*             2 = Van Ulden - Holtslag relation
*             3 = Carl relation
*
*     ims  :  set parameters for Momentum Flux (Stable Conditions)
*             0 = No changes
*             1 = Businger relation
*             2 = Van Ulden - Holtslag relation
*             3 = Beljaars - Holtslag relation
*
*     ihc  :  set parameters for Heat Flux (Convective Conditions)
*             0 = No changes
*             1 = Businger relation
*
*     ihs  :  set parameters for Heat Flux (Stable Conditions)
*             0 = No changes
*             1 = Businger relation
*             2 = Van Ulden - Holtslag relation
*             3 = Beljaars - Holtslag relation
*
*     The give parameters are used in a labeled COMMON /set_authors/
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set
      imc_set=imc
      ims_set=ims
      ihc_set=ihc
      ihs_set=ihs
      return
      end

c& @  ================================================================
      Subroutine Hmix_1(dtime,gg,N,us,h0,Tm,hmix,ier)
c     ================================================================
*     This routine computes the hourly convective mixing height accor-
*     ding to Gryning e Batchvarova model (1991).
*
*     Input
*         dtime  = time step (hour)
*         gg     = lapse rate above hmix (øK/m)
*         N      = number of daily time steps
*         us     = array of daily friction velocity (m/s)
*         h0     = array of sensible heat flux (W/mq)
*         Tm     = array of average air temperature (øK)
*
*     output
*         hmix   = array of thermal mixing height (m)
*         ier    = 0 if no errors detected
*                  1 if errors were detected in one or more step
*
c     By:   Servizi Territorio coop a r.l. (08 june 1992)
c     -----------------------------------------------------------------
      REAL us(N),h0(N),Tm(N),hmix(N)
c

      ier = 1

      if(dtime .LT.0. .OR. gg.LT.0. .OR. N.LT.0 ) return
      do i=1,N
         if(H0(i).GT.0.) then
c                        convective situations in daytime exist
                         ier = 0
                         exit
         endif
      enddo

      if(ier.EQ.1) return

      do i=1,N
       hmix(i)=0.
      enddo

      do i = 1,N
             if(H0(i).GT.0.) then
c                            start of convective situations
                             i1 = i
                             EXIT
             endif
      enddo
      i2=N
      do i = i1,N
             if(H0(i).LT.0.) then
c                            stop  of convective situations
                             i2 = i-1
                             EXIT
             endif
      enddo

      do i=i1,i2
           hm  = hmix(i-1)
           us1 = us(i-1)
           us2 = us(i)
           Tm1 = Tm(i-1)
           Tm2 = Tm(i)
           h01 = h0(i-1)
           h02 = h0(i)
           if(h01.LT.0.) h01 = 0.
           if(h02.LT.0.) h02 = 0.
           if(h02.gt.0.) then
             hmix(i) = Rise1_to(dtime,h01,h02,us1,us2,Tm1,Tm2,gg,hm)
           else
             hmix(i)=hmix(i-1)
           endif
      enddo

      return
      end

c     =================================================================
      Function Rise1_to(dtime,h01,h02,us1,us2,Tm1,Tm2,gg,hm)
c     =================================================================
      DATA n_step/60/

      Rise1_to=-9999.
      dt   = dtime*3600./n_step
      dh   = (h02-h01)/n_step
      us   = (us1+us2)/2.
      Tm   = (Tm1+Tm2)/2.
      rc   = RHOCP(Tm)
      if(rc.eq.-9999.) return

      hh   = h01
      hmm  = hm
      do i=1,n_step
           hh  = hh+dh
           hk1 = dt * F1(rc,Tm,gg,us,hh,hmm)
           hk2 = dt * F1(rc,Tm,gg,us,hh,hmm+hk1/2.)
           hk3 = dt * F1(rc,Tm,gg,us,hh,hmm+hk2/2.)
           hk4 = dt * F1(rc,Tm,gg,us,hh,hmm+hk3/2.)
           hmm = hmm + (hk1+2.*(hk2+hk3)+hk4)/6.
      enddo

      Rise1_to = hmm

      return
      end

      !     =================================================================
      Function F1(rc,Tm,gg,us,h0,hm)
      !     =================================================================
          data hk/0.4/,g/9.81/,A/0.2/,B/2.5/,C/8/
          hL   = -rc*Tm*us*us*us/(hk*g*h0)
          cost = B*hk*hL
          f1   = (1.+2.*A)*hm-2.*cost
          f2   = gg*g*((1.+A)*hm-cost)
          F1 = hm*hm/f1 + C*us*us*Tm/f2
          F1 = h0/(rc*gg*F1)
      END Function F1


c& @  ================================================================
      Subroutine HMIX_2(dtime,gg,N,h0,Tm,hmix,ier)
c     ================================================================
*     This routine computes the hourly mixing height according to the
*     thermodynamic mixed-layer growth model. (Stull)
*
*     Input
*         dtime  = time step (hour)
*         gg     = lapse rate above hmix (øK/m)
*         N      = number of daily time steps
*         h0     = array of sensible heat flux (W/mq)
*         Tm     = array of mean temperature (øK)
*
*     output
*         hmix   = array of thermal mixing height (m)
*         ier    =0 if no errors detected
*                =1 if errors were detected in one or more steps
*
c     By:   Servizi Territorio coop a r.l. (08 june 1992)
c     -----------------------------------------------------------------
      REAL h0(N),Tm(N),hmix(N)
c
      ier = 1
      if(dtime.LT.0. .OR. gg.LT.0. .OR. N.LT.0.) return
      do i=1,N
         if(H0(i).GT.0.) then
c                        convective situations in daytime
                         ier = 0
                         exit
         endif
      enddo
      if(ier.EQ.1) return

      do i = 1,N
             if(H0(i).GT.0.) then
c                            start of convective situations
                             i1 = i
                             EXIT
             endif
      enddo
      i2=N
      do i = i1,N
             if(H0(i).LT.0.) then
c                            stop  of convective situations
                             i2 = i-1
                             EXIT
             endif
      enddo

      do i=i1,i2
           hm  = hmix(i-1)
           Tm1 = Tm(i-1)
           Tm2 = Tm(i)
           h01 = h0(i-1)
           h02 = h0(i)
           if(h01.LT.0.) h01 = 0.
           if(h02.LT.0.) h02 = 0.
           hmix(i) = Rise2_to(dtime,h01,h02,Tm1,Tm2,gg,hm)
      enddo

      return
      end

c     =================================================================
      Function RISE2_TO(dtime,h01,h02,Tm1,Tm2,gg,hm)
c     =================================================================
      DATA n_step/60/

      RISE2_to=-9999.
      dt   = dtime*3600./n_step
      dh   = (h02-h01)/n_step
      Tm   = (Tm1+Tm2)/2.
      rc   = RHOCP(Tm)
      if(rc.eq.-9999.) return

      hh   = h01
      hmm  = hm
      do i=1,n_step
           hh  = hh+dh
           hmm = SQRT(hmm*hmm+2.4*hh/(rc*gg)*dt)
      enddo

      RISE2_TO = hmm

      return
      end

c& @  ==================================================================
      Function H_ground(T,zt,vel,zu,z0,rn)
c     ==================================================================
*     This routine computes the ground heat flux.
*
*     Input:
*              T   = temperature (Kelvin) measured at zt.
*              zt  = heigth where the temperature is measured (m)
*              vel = wind velocity (m/s)
*              zu  = heigth where wind speed is measured (m)
*              z0  = roughness length (m)
*              rn  = net radiation (W/mq)
*
*     Output:
*              H_ground = Ground heat flux (W/mq)
*                         -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data g/9.81/, tstar/0.08/, ag/5./, hk/0.4/, sigma/5.67e-08/

      if(zt.LT.0. .OR. zu.LT.0. .OR. z0.LT.0. OR. T.LT.0. .OR.
     %   vel.LT.0.) then
                    H_ground = -9999.
                    return
      endif

      if(rn.le.0.) then
c                  stable conditions (night time)
                   alu = alog(zu/z0)
                   al0 = 5.* zu/alu
                   aln = hk * vel*vel * T/(2.*g*tstar * alu*alu)
                   if(aln.ge.2.*al0) then
                                 al = (aln-al0)+SQRT(aln*(aln-2.*al0))
                   else
                                 al = SQRT(al0*aln/2.)
                   endif
                   dt = tstar/hk*(alog(zt/z0*10.)+5.*zt/al)-0.0098*zt
                   H_ground = -ag*dt
                   return
      else
c                  convective conditions (daytime)
                   s        = exp(0.055*(T-279.))
                   ch       = 0.38/(1.+s)
                   dt       = ch*rn/(4*sigma*T**3)
                   H_ground = ag*dt
                   return
      endif
      return
      end

c&@   ==================================================================
      Subroutine PBL_5 (nn,zz,uu,z0,idep,T,  us,ts,h0,hlm)
c @   ==================================================================
*     This routine computes the Surface Layer parameters starting from
*     the wind speed profile in the Surface Layer and the surface tempe-
*     rature.
*
*     Input:
*                 nn   = # of heights in the profile array
*                 zz   = heights in the profile  array (nn values) (m)
*                 uu   = wind speed in the profile array (nn val.) (m/s)
*                 z0   = roughness lenght (m)
*                 idep = 0 if no screening of data, 1 if yes
*                 T    = at ground level temperature (Kelvin)
*     Output:
*                 us   = friction velocity (m/s)
*                 ts   = scale temperature (Kelvin)
*                 h0   = sensible heat flux (W/m2)
*                 hlm  = 1/Monin Obukhov length (1/m)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      real  z[allocatable](:)
      real  u[allocatable](:)
      real zz(nn),uu(nn)

      data hk/0.4/,g/9.81/
      data epsi/0.2 /

      us  = -9999.
      ts  = -9999.
      h0  = -9999.
      hlm = -9999.

      if(nn.le.3) return

      allocate (z(nn),u(nn))

      do i=1,nn
          z(i) = zz(i)
          u(i) = uu(i)
          n    = nn
      enddo

c     costruisco il Guess iniziale:
c     per a0 ipotesi di adiabaticita', b0= 16./1000.
      a0=0.
      do i=1,n
         alz0 = alog(z(i)/z0)
         a0   = a0+u(i)/alz0
      enddo
      a0  = a0/n
      b0  = 16./1000.

      if(idep.eq.1) then
c        test di gestibilita' dei dati.
c        metodo dello scostamento dalla adiabaticita'
c        a) definisco il seguente parametro medio xm
                    xm=0.
                    do i=1,nn
                       alz0 = alog(zz(i)/z0)
                       xm   = xm+uu(i)/alz0
                    enddo
                    xm=xm/nn
c        b) definisco lo scostamento percentuale dal profilo adiabatico
                    n=0
                    do i=1,nn
                       alz0 = alog(zz(i)/z0)
                       x    = uu(i)/alz0
                       ei   = abs((x-xm)/xm)
                       if(ei.lt.epsi) then
c                                    il dato si tiene
                                     n    = n+1
                                     z(n) = zz(i)
                                     u(n) = uu(i)
                       endif
                    enddo
c                  if(n.lt.3) then
c                     deallocate (z,u)
c                     return
c                  endif
      endif


c     calcolo  in condizioni convettive
      if(n.lt.3) then
        r_c=9999.
      else
        a00 = a0
        b00 = b0
        ics=0
        call Gauss(n,z,u,z0,ics,a00,b00,a_c,b_c,R_c)
      endif

c     calcolo  in condizioni stabili (idep non ha effetto)
      a00=a0
      b00=b0
      ics=1
      call Gauss(nn,zz,uu,z0, ics,a00,b00,a_s,b_s,R_s)

c     Calcolo il valore residuo in condizioni adiabatiche
c       (idep non ha effetto)
      R_a= R_adiab(nn,zz,uu,z0,a0)

c     scelgo il risultato che presenta il minore valore residuo
c     convectivity
      a   =  a_c
      b   =  b_c
      hlm = -b/16.
      R   =  R_c

c     stability
      if(R.GT.R_s) then
                   a   = a_s
                   b   = b_s
                   hlm = b
                   R   = R_s
      endif

c     adiabatic situation
      if(R.GT.R_a) then
                   a   = a0
                   b   = 0.
                   hlm = 0.
                   R   = R_a
      endif

      us = a*hk
      ts = T * us*us* hlm /hk/g
      h0 =-Rhocp(T)*us*ts

      deallocate  (z,u)

      return
      end

c     -----------------------------------------------------------------
      subroutine Gauss(n,z,u,z0,ics, a0,b0,a,b,R)
c     -----------------------------------------------------------------
c     compie le  iterazioni del metodo Newton-Gauss e ottiene i nuovi
c     valori dei parametri a e b.
c     Termina quando i parametri si ricoprono entro ermax
c     ics= flag per situazioni convettive (ics=0) e stabili (ics=1)

      real z(n),u(n)

      ermax = 0.01
      nmax  = 20
      do i=1,nmax
        call grad_hess(n,z,u,z0,ics,a0,b0, Ga,Gb, Haa, Hbb, Hab, Det,R)
        a = a0 - (Hbb*Ga - Hab*Gb)/Det
        b = b0 - (Haa*Gb - Hab*Ga)/Det
c       protezione rispetto al valore che puo' assumere b
c       nota: il significato di b e' diverso in condizioni stabili
c             o convettive. Tale che b > 0 sempre.
        if(b.le.0.) then
                    b=0.
                    R=999999.
                    return
        endif

        erra= abs((a-a0)/a0)
        errb= abs((b-b0)/b0)
        if(amax1(erra,errb).le.ermax) exit
        a0 = a
        b0 = b
      enddo
c     calcolo il residuo piu' aggiornato
      call grad_hess(n,z,u,z0,ics,a ,b , Ga,Gb, Haa, Hbb, Hab, Det,R)

      return
      end

c     -----------------------------------------------------------------
      subroutine grad_hess(n,z,u,z0,ics,a,b,Ga,Gb,Haa,Hbb, Hab,Det,R)
c     -----------------------------------------------------------------
c     calcola i residui, il gradiente e l'Hessiano della funzione
c     da minimizzare
c
c     n   = numero di elementi del vettore (= n. di quote)
c     z   = array delle quote
c     u   = array delle velocita' (profilo)
c     z0  = roughness lenght
c     ics = flag per situazioni convettive (ics=0) e stabili (ics=1)
c     a   = u*/k
c     b   = -16./L
c     Ga  = gradiente del Funzionale rispetto ad a
c     Gb  = gradiente del Funzionale rispetto a  b
c     Haa = Elemento a11 dell'Hessiano
c     Hbb = Elemento a22 dell'Hessiano
c     Hab = Elemento a12 e A21 dell'Hessiano
c     det = determinate della matrice Hessiana
c     R   =  somma dei quadrati dei residui
c     -----------------------------------------------------------------
      real  z(n), u(n)

      Ga  = 0.
      Gb  = 0.
      Haa = 0.
      Hbb = 0.
      Hab = 0.
      R   = 0.

      do i= 1,n
         if(ics.eq.0) then
                      call f_df_c(z(i),u(i),a,b,z0,fi,dfai,dfbi)
         elseif(ics.eq.1) then
                     call f_df_s(z(i),u(i),a,b,z0,fi,dfai,dfbi)
         endif
         ga  = ga  + fi*dfai
         gb  = gb  + fi*dfbi
         Haa = Haa + dfai*dfai
         Hbb = Hbb + dfbi*dfbi
         Hab = Hab + dfai*dfbi
         R   = R   + fi*fi
      enddo

      ga  = 2.*ga
      gb  = 2.*gb
      Haa = 2.*Haa
      Hbb = 2.*Hbb
      Hab = 2.*Hab

c     calcolo anche il Determinate
      Det = Haa*Hbb - Hab*Hab

      return
      end

c     -----------------------------------------------------------------
      subroutine f_df_c(z,u,a,b,z0,fi,dfai,dfbi)
c     -----------------------------------------------------------------
c     calcola il valore della funzione differenza tra u misurata e u
c     calcolata (fi) in condizioni CONVETTIVE.
c     Inoltre ne calcola anche le derivate rispetto ad a (u*/k), cioe' dfai
c     e rispetto a b (-16/L), cioe' dfbi.

      alz0 = alog(z/z0)
      bb   = b
c     valore della funzione
      if(bb.lt.0.) then
                   bb = 0.
      endif

      ucalc = a*(alz0 + 1.-(1.+z*bb)**0.25)
      fi    = u - ucalc
      dfai  = -(alz0+ 1. -(1.+z*bb)**0.25)
      dfbi  = a*z/(4.*(1.+z*bb)**0.75)

      return
      end

c     -----------------------------------------------------------------
      subroutine f_df_s(z,u,a,b,z0,fi,dfai,dfbi)
c     -----------------------------------------------------------------
c     calcola il valore della funzione differenza tra u misurata e u
c     calcolata (fi) in condizioni STABILI.
c     Inoltre ne calcola anche le derivate rispetto ad a (u*/k), cioe' dfai
c     e rispetto a b (-16/L), cioe' dfbi.

      alz0= alog(z/z0)

c     valore della funzione
      ucalc = a*(alz0 + 17.*(1.-exp(-0.29*z*b)))
      fi    = u - ucalc
      dfai  = - (alz0+  17.*(1.-exp(-0.29*z*b)))
      dfbi  = -4.93*a*z*exp(-0.29*z*b)

      return
      end

c     -----------------------------------------------------------------
      Function R_adiab(n,z,u,z0,a0)
c     -----------------------------------------------------------------
c     calcola il residuo in condizioni adiabatiche
      real z(n),u(n)

      R_adiab=0.
      do i=1,n
        R_adiab= R_adiab + (u(i)-a0*alog(z(i)/z0))**2
      enddo

      return
      end



c& @  =================================================================
      Subroutine PBL_1 (z0,zr,vel,z1,z2,T1,T2,  us,ts,h0,hlm)
c     =================================================================
*     This routine computes the SL parameters by only one  wind speed
*     measurement at zr level and two temperature measurements at z1
*     and z2 levels (z2>z1).
*
*     Input data:
*              z0    = roughness length (m)
*              zr    = heigth where wind speed is measured (m)
*              vel   = average wind speed at zr  (m/s)
*              z1    = height of T1 measurement (m)
*              z2    = height of T2 measurement (m)
*              T1    = air potential temperature at z1 (Kelvin)
*              T2    = air potential temperature at z2 (Kelvin)
*
*     Output data:
*                us   = friction velocity (m/s)
*                ts   = scale temperature (Kelvin)
*                h0   = sensible heat flux (W/m2)
*                hlm  = 1/Monin Obukhov length (1/m)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     -------------------------------------------------------------------
      data hk/0.4/, g/9.81/
c
      us  = -9999.
      ts  = -9999.
      H0  = -9999.
      hlm = -9999.

      if(zr.LT.z0 .OR. z2.LT.z1) return

      alu  = alog(zr/z0)
      alt  = alog(z2/z1)
      T    = (T1+T2)/2.
      dtet = T2-T1

c     rhocp is computed
      rc = RHOCP(T)
      if(rc .EQ.-9999.) return

      if(vel.LT.0.) return

      if(dtet.lt.0) then
c                   convective condition
c                   fist guess for us and ts
                    us  = hk * vel / alu
                    ts  = hk * dtet /alt
                    hlm = HLM_1(T,us,ts)
                    if(hlm.EQ.-9999.) return

c                   iterative procedure
                    do i=1,20
                         us  = USTAR_1(z0,zr,vel,hlm)
                            if(us.EQ.-9999.) return
                         ts  = TSTAR_1(z1,z2,dtet,hlm)
                            if(ts.EQ.-9999.) return
                         hlm = HLM_1(T,us,ts)
                            if(hlm.EQ.-9999.) return
                         h0  = -rc * us * ts
                   enddo
      else
c                  stable  condition
                   a  = T* vel*vel/(g*dtet)
                   zz = 0.001
                   do i=1,20
                        fh1 = PSIH_S(z1,zz)
                        fh2 = PSIH_S(z2,zz)
                        fm1 = PSIM_S(z0 ,zz)
                        fm2 = PSIM_S(zr ,zz)
                              if(fh1.eq.-9999.) return
                              if(fh2.EQ.-9999.) return
                              if(fm1.EQ.-9999.) return
                              if(fm2.EQ.-9999.) return
                        zz  = (alu-fm2+fm1)**2 / (a*(alt-fh2+fh1))
                   enddo
                   hlm = zz
                   us  = USTAR_1(z0,zr,vel,hlm)
                         if(us.eq.-9999.) return
                   ts  = TSTAR_1(z1,z2,dtet,hlm)
                         if(ts.eq.-9999.) return
                   h0  = -rc * us * ts
      endif
      return
      end

c& @  =================================================================
      Subroutine PBL_2 (zu1,zu2,vel1,vel2,zt1,zt2,T1,T2, us,ts,h0,hlm)
c     =================================================================
*     This routine computes the SL parameters by two wind measures at
*     zu1 and zu2 levels (zu2>zu1) and two temperature measures at zt1
*     and zt2 levels (zt2>zt1).
*
*     Input data:
*              zu1   = lower height where wind speed is measured (m)
*              zu2   = upper height where wind speed is measured (m)
*              vel1  = wind speed at zu1  (m/s)
*              vel2  = wind speed at zu1  (m/s)
*              zt1   = height of T1 measurement (m)
*              zt2   = height of T2 measurement (m)
*              T1    = air potential temperature at z1 (Kelvin)
*              T2    = air potential temperature at z2 (Kelvin)
*
*     Output data:
*                us   = friction velocity (m/s)
*                ts   = scale temperature (Kelvin)
*                h0   = sensible heat flux (W/m2)
*                hlm  = 1/Monin Obukhov length (1/m)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     -------------------------------------------------------------------
      data hk/0.4/, g/9.81/
c
      us  = -9999.
      ts  = -9999.
      H0  = -9999.
      hlm = -9999.

      if(zu2.LT.zu1 .OR. zt2.LT.zt1) return
c
      alu  = alog(zu2/zu1)
      alt  = alog(zt2/zt1)
      Tm   = (T1+T2)/2.
      dtet = T2-T1
      du   = vel2 - vel1
      if(du.LT.0.01) du = 0.01

c     rhocp is computed
      rc = RHOCP(Tm)
      if(rc.eq.-9999.) return

      if(dtet.lt.0) then
c                   convective condition
c                   fist guess for us and ts
                    us  = hk * du   / alu
                    ts  = hk * dtet /alt
                    hlm = HLM_1(Tm,us,ts)
                         if(hlm.eq.-9999.) return

c                   iterative procedure
                    do i=1,20
                         us  = USTAR_1(zu1,zu2,du,hlm)
                               if(us .EQ.-9999.) return
                         ts  = TSTAR_1(zt1,zt2,dtet,hlm)
                               if(ts .EQ.-9999.) return
                         hlm = HLM_1(Tm,us,ts)
                               if(hlm.EQ.-9999.) return
                         h0  = -rc * us * ts
                   enddo
      else
c                  stable  condition
                   a  = Tm* du*du/(g*dtet)
                   zz = 0.001
                   do i=1,20
                        fh1 = PSIH_S(zt1,zz)
                        fh2 = PSIH_S(zt2,zz)
                        fm1 = PSIM_S(zu1,zz)
                        fm2 = PSIM_S(zu2,zz)
                              if(fh1.eq.-9999.) return
                              if(fh2.EQ.-9999.) return
                              if(fm1.EQ.-9999.) return
                              if(fm2.EQ.-9999.) return
                        zz  = (alu-fm2+fm1)**2 / (a*(alt-fh2+fh1))
                   enddo
                   hlm = zz
                   us = USTAR_1(zu1,zu2,du,hlm)
                         if(us.eq.-9999.) return
                   ts = TSTAR_1(zt1,zt2,dtet,hlm)
                         if(ts.eq.-9999.) return
                   h0 = -rc * us * ts
      endif

      return
      end

c&@   ==================================================================
      Subroutine PBL_4 (T,us,h0,  ts,hlm)
c @   ==================================================================
*     This routine computes the Surface Layer parameters starting from
*     the friction velocity and sensible heat flux.
*
*     Input data:
*              T   = air temperature (Kelvin)
*              us  = friction velocity (m/s)
*              h0  = sensible heat flux (W/m2)
*
*     Output data:
*                ts   = scale temperature (Kelvin)
*                hlm  = 1/Monin Obukhov length (1/m)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      ts  = -9999.
      hlm = -9999.
      if(T.LT.0. .OR. us.LT.0.) return

c     rhocp is computed
      rc = RHOCP(T)
      if(rc.eq.-9999.) return

c     scale temperature
      ts = TSTAR_0(rc,us,h0)
      if(ts.eq.-9999.) return

c     1/Monin Obukhov length is computed
      hlm = HLM_1(T,us,ts)
      if(hlm.eq.-9999.) return

      return
      end

C&@   ================================================================
      Subroutine PBL_3 (z0,zr,vel,T,h0_in, us,ts,h0,hlm)
C     ================================================================
*     This routine computes the SL parameters by a wind measure at zu
*     level and h0 (sensible heat flux) only in daytime situations.
*     NOTE: if h0 in input is negative (stable conditions) it will
*           be recalculated (modified) by the routine.
*           During daytime hours (h0_in>0.) will have h0_in=h0.
*           If you have a suitable value for h0 (i.e. from measurements)
*           then you'll find better to use PBL_3A routine.
*
*     Input data:
*              z0    = roughness length (m)
*              zr    = height where wind speed is measured (m)
*              vel   = wind speed at zr  (m/s)
*              T     = mean air temperature (Kelvin)
*              h0_in = sensible heat flux (W/m2)
*
*     Output data:
*                us   = friction velocity (m/s)
*                ts   = scale temperature (Kelvin)
*                h0   = sensible heat flux (W/m2) for daytime hours,
*                hlm  = 1/Monin Obukhov length (1/m)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     -------------------------------------------------------------------
      data hk/0.4/, gamma/4.7/, aa/1100./

      us  = -9999.
      ts  = -9999.
      hlm = -9999.

      if(z0.LT.0. .OR. zr.LT.z0 .OR. vel.LT.0. .OR. T.LT.0.) return

c     rhocp is computed
      rc = RHOCP(T)
      if(rc .EQ.-9999.) return

      alu = alog(zr/z0)

      if(h0_in.GT.0.) then
c                  convective situation
                   h0=h0_in
c                  first guess for us
                   us  = hk * vel / alu
                   hlm = HLM_2(rc,T,us,h0)
                       if(hlm.eq.-9999.) return
c
c                  iterative procedure
                   do i=1,20
                        us  = USTAR_1(z0,zr,vel,hlm)
                              if(us.eq.-9999.) return
                        hlm = HLM_2(rc,T,us,h0)
                              if(hlm.EQ.-9999.) return
                   enddo
                   ts  = TSTAR_0(rc,us,h0)
                              if(ts.eq.-9999.) return
      else
c                  stable situation
                   zm=zr- 4.*z0
                   cdn= hk/alu
                   u0 = gamma * zm/(hk*aa)
                   c= 1.- 4.*u0/(cdn*vel*vel)
                   if(c.lt.0.) c=0.
                   us= 0.5*cdn*vel*(1.+c**0.5)
                   h0= -rc*us*T/(9.81*hk*aa)
                   hlm=1./(aa*us*us)
                   ts= -h0/(rc*us)

      endif

      return
      end

!	================================================================
      Subroutine PBL_33(iland,z0,d,zr,vel,T,Rn,cloud,us,Ts,H0,hlm)
!     ================================================================
      REAL alpha(6)

      DATA alpha/0.1,0.3,0.5,0.7,1.,1.4/
      DATA beta/20./, hk/0.4/, gg/9.81/
!     -----------------------------------------------------------------
	IF(z0 .LT.0.) z0 = 0.1
      if(iland.LT.1  .OR. iland.GT.6) iland = 4

      hlmin    = 1./5.  
      ustarmin = 0.05     
      rc       = 1305.*273.16/T
      rground  = 0.8

      alu    = ALOG((zr-d)/z0)
      tt     = T-273.15
      S      = 1.05*EXP( (6.42-tt)/17.78 )
      alp    = alpha(iland)

      H0 = ((1-alp)+S)/(1+S) * rground*Rn - beta

	IF(H0.GT.0.) Then
		usn = hk*vel/alu
		zz0 = z0/zr
		aln = ALOG(z0/zr)
		IF(zz0 .LE.0.01) Then
					  d1 = 0.128+0.005*aln
		Else
					  d1 = 0.107
		Endif
		d2 = 1.95+32.6*(zz0)**0.45
		IF(h0 .LE. 0.) Then
				  d3 = 0.
		Else
				  d3 = H0/rc * (hk*gg*zr)/(T*usn**3)
		Endif
		us  = usn * (1. + d1*ALOG(1. + d2*d3))
		IF(us .LT. ustarmin) us = ustarmin
		Ts  = -H0/rc/us
		hlm = hk*gg/T * Ts/us**2
      Else
		Ts1    = 0.09*(1. - 0.5*cloud**2)
		Ts2    = hk*T*vel**2/(18.8*zr*gg*alu)
		Ts     = AMIN1(ts1,ts2)
		us_min = hk/alu*vel  
		uss    = 0.5*hk*vel/alu
		uuu    = 1.- 4.* 4.7*gg*zr*Ts*alu/(hk*T*vel**2)
		IF(uuu.LE.0.) Then
			hlm = hlmin  
			us  = hk*vel/(alu + 4.7*zr*hlm)
			IF(us .LT.us_min) us = us_min
			H0  = -rc*us*ts
		Else
			uuu = SQRT(uuu)
			us  = uss * (1+uuu)
			IF(us .LT. us_min) us = us_min
			h0  = -rc*us*ts
			hlm = hk*gg/T * Ts/us**2
		Endif
      Endif

      RETURN
      END

!     =================================================================
      Function Hmix_new (dtime,H0,us,Tm,rc,hold)
!     =================================================================
      DATA n_step/60/
!     -----------------------------------------------------------------
      Hmix_new = -9999.
      dt       = dtime/n_step

      if(rc.eq.-9999.) RETURN

	ggmm = 0.005
      hmm  = hold
      DO i=1,n_step
           hk1  = dt * F(rc,Tm,ggmm,us,H0,hmm)
           hk2  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk1/2.)
           hk3  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk2/2.)
           hk4  = dt * F(rc,Tm,ggmm,us,H0,hmm+hk3/2.)
           hmm  = hmm + (hk1+2.*(hk2+hk3)+hk4)/6.
      EndDO

      Hmix_new = hmm

      RETURN
      END

!     =================================================================
      Function F(rc,Tm,gg,us,h0,hm)
!     =================================================================
      data hk/0.4/,g/9.81/,A/0.2/,B/2.5/,C/8/

      hL   = -rc*Tm*us*us*us/(hk*g*h0)
      cost = B*hk*hL
      f1   = (1.+2.*A)*hm-2.*cost
      f2   = gg*g*((1.+A)*hm-cost)

      F    = hm*hm/f1 + C*us*us*Tm/f2

      F = h0/(rc*gg*F)

      RETURN
      END
!
c& @  =================================================================
      Function PSIH_C(zr,hl)
c     =================================================================
*     This routine returns the value of the universal heat function  in
*     convective conditions starting from the reference height and  the
*     Monin-Obukhov length.
*
*     Input data:
*                iaut = 1 -> Businger relation (DEFAULT)
*        NOTE! iaut must be set up using Set_Author_pbl Routine.
*
*                zr   = reference height (m)
*                hl   = inverse of Monin Obukhov length (1/m)
*
*     Output data:
*                PSIH_C = value of universal function in convective
*                         conditions. (-9999. IF ERROR)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set

      PSIH_C = -9999.
      if(zr.LE.0. .OR. hl.GT.0.) return

      zl = zr*hl

      if(ihc_set.ne.1) then
               iaut=1
      else
               iaut=ihc_set
      endif

      go to (1),iaut

c  ->    Businger relation
  1      y      = SQRT(1.-16.*zl)
         PSIH_C = 2.*alog((1.+y)/2.)
         return

      end

c& @  =================================================================
      Function PSIH_S(zr,hl)
c     =================================================================
*     This routine returns the value of the universal heat function  in
*     stable conditions starting from the reference height and the
*     Monin-Obukhov length.
*
*     Input data:
*                iaut = 1 -> Businger relation
*                       2 -> van Ulden Holtslag relation (DEFAULT)
*                       3 -> Beljaars Holtslag relation
*        NOTE! iaut must be set up using Set_Author_pbl Routine.
*
*                zr   = reference height (m)
*                hl   = inverse of Monin Obukhov length (1/m)
*
*     Output data:
*                PSIH_S = value of universal function in stable
*                         conditions. (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set
      DATA a/1./, b/0.667/, c/5./, d/0.35/

      PSIH_S = -9999.
      if(zr.LE.0. .OR. hl.LT.0.) return

      zl = zr*hl

      if(ihs_set.ne.1  .or.
     %   ihs_set.ne.2  .or.
     %   ihs_set.ne.3) then
               iaut=2
      else
               iaut=ihs_set
      endif

      go to (1,2,3) iaut

c ->     Businger relation
 1       PSIH_S = -5.*zl
         return

c ->     van Ulden Holtslag
 2       PSIH_S = -17. * (1.-exp(-0.29*zl))
         return

c ->     Beljaars Holtslag
 3       exz    = exp(-d*zl)
         c1     = 1.+0.666667*a*zl
         c2     = zl-c/d
         c3     = b*c/d
         PSIH_S = -c1**1.5 - b*c2*exz - c3 + 1.
         return

      end

c& @  =================================================================
      Function PSIM_C(zr,hl)
c     =================================================================
*     This routine returns the value of the universal momentum function
*     in convective conditions starting from the reference height and
*     the Monin-Obukhov length.
*
*     Input data:
*                iaut = 1 -> Businger relation (DEFAULT)
*                       2 -> van Ulden Holtslag relation
*                       3 -> Carl relation
*        NOTE! iaut must be set up using Set_Author_pbl Routine.
*
*                zr   = reference height (m)
*                hl   = inverse of Monin Obukhov length (1/m)
*
*     Output data:
*                PSIM_C = value of universal function in convective
*                         conditions. (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set

      PSIM_C = -9999.
      if(zr.LE.0. .OR. hl.GT.0.) return

      zl = zr*hl

      if(imc_set.ne.1  .or.
     %   imc_set.ne.2  .or.
     %   imc_set.ne.3) then
               iaut=1
      else
               iaut=imc_set
      endif

      go to (1,2,3),iaut

c  ->    Businger relation
 1       cost   = 1.570796
         x      = (1.-16.*zl)**0.25
         y      = (1.+x)/2.
         PSIM_C = ALOG((1.+x*x)/2.*y*y) - 2.*ATAN(x) + cost
         return

c  ->    van Ulden Holtslag relation
 2       PSIM_C = (1.-16.*zl)**0.25 - 1.
         return

c  ->    Carl relation
 3       cost   = 0.165881
         c1     = 1.5
         c2     = 1.7320508
         x      = (1.-16.*zl)**0.3333
         PSIM_C = c1 * ALOG(x*x+x+1.) - c2 * ATAN((2.*x+1)/c2) + cost
         return
      end

c& @  =================================================================
      Function PSIM_S(zr,hl)
c     =================================================================
*     This routine returns the value of the universal momentum function
*     in stable conditions starting from the reference height  and  the
*     Monin-Obukhov length.
*
*     Input data:
*                iaut = 1 -> Businger relation
*                       2 -> van Ulden Holtslag relation (DEFAULT)
*                       3 -> Beljaars Holtslag relation
*        NOTE! iaut must be set up using Set_Author_pbl Routine.
*
*                zr   = reference height (m)
*                hl   = inverse of Monin Obukhov length (1/m)
*
*     Output data:
*                PSIM_S = value of universal function in stable
*                         conditions. (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set
      DATA a/0.7/, b/0.75/, c/5./, d/0.35/

      PSIM_S = -9999.
      if(zr.LE.0. .OR. hl.LT.0.) return

      if(ims_set.ne.1  .or.
     %   ims_set.ne.2  .or.
     %   ims_set.ne.3) then
               iaut=2
      else
               iaut=ims_set
      endif

      zl = zr*hl

      go to (1,2,3),iaut

c  ->    Businger relation
  1      PSIM_S = -5.*zl
         return

c  ->    van Ulden Holtslag
  2      PSIM_S = -17. * (1.-exp(-0.29*zl))
         return

c  ->    Beljaars Holtslag relation
  3      PSIM_S = -a*zl-b*(zl-c/d)*exp(-d*zl)-b*c/d
         return

      end

C&@   ==================================================================
      Function RHOCP(T)
C     ==================================================================
*     This routine returns the value of the product of air density and
*     constant pressure specific heat.
*
*     Input data:
*                T = air temperature (Kelvin)
*
*     Output data:
*                RHOCP = density * specific heat
*                        -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      if(T.LT.0.) then
                  RHOCP = -9999.
                  return
      endif
c
      RHOCP = 1305. * 273.16/T
C
      return
      end

c& @  ==================================================================
      Function SIGMA_V (z,us,ws,hmix,zr,svr)
C     ==================================================================
*     This routine computes the horizontal wind speed standard deviation
*     according to results obtained by Híjstrup (1981,1982) e Berkowiczc
*     Prahm (1982).
*
*     Input:
*             z    = height (m)
*             us   = friction velocity (m/s)
*             ws   = free convection scaling velocity (m/s)
*             hmix = mixing height (m)
*             zr   = reference height (m)
*             svr  = standard deviation of horizontal wind speed
*                    measured at zr  (m/s)
*                    If no sv measured, zr=svr=0.
*
*     Output:
*             sigma_v= standard deviation of horizontal wind velocity at
*                      height z (m/s)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      if(z.LT.0. .OR. us.LT.0. .OR. hmix.LT.0. OR.
     %   zr.LT.0. .OR. svr.LT.0.) then
                                  sigma_v = -9999.
                                  return
      endif

      if(zr.gt.0. .AND. svr.gt.0.) then
                                   svrc    = SIGMAV (zr,us,ws,hmix)
                                   f       = svr/svrc
                                   SIGMA_V = f * SIGMAV (z,us,ws,hmix)
      else
                                   SIGMA_V = SIGMAV (z,us,ws,hmix)
      endif

      return
      end

c     =================================================================
      Function SIGMAV (z,us,ws,hmix)
c     =================================================================
      data c1,c2,c3,c4 /0.38,2.70,2.80,1.643/
c
      if(ws.gt.0.) then
c                  convective situation
                   s1 = c1 * ws*ws
                   if(z.lt.hmix) then
                                s2 = c2 * us*us*(1.-z/hmix)*(1.-z/hmix)
                                s3 = (1.+c3*z/hmix)**0.66667
                                s1 = s1+s2/s3
                   endif
                   sigmav = sqrt(s1)
      else
c                  stable situation
                   if(z.lt.0.9*hmix) then
                                     s1     = c4 * us * (1.-z/hmix)
                                     sigmav = s1/(1.+c3*z/hmix)**0.333
                   else
                                     sigmav = 0.1053*us
                   endif
      endif
c
      return
      end


C&@   ==================================================================
      Function SIGMA_W (z,us,ws,hmix,zr,swr)
C     ==================================================================
*     This routine computes the vertical wind velocity standard deviation
*     according to results obtained by Híjstrup (1981,1982) e Berkowicz
*     Prahm (1982).
*
*     Input:
*             z    = height (m)
*             us   = friction velocity (m/s)
*             ws   = free convection scaling velocity (m/s)
*             hmix = mixing height (m)
*             zr   = reference height (m)
*             swr  = standard deviation of vertical wind speed
*                    measured at zr  (m/s)
*                    If no sw measured, zr=swr=0.
*
*     Output:
*             sigma_w= standard deviation of vertical wind velocity at
*                      height z (m/s)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      if(z.LT.0. .OR. us.LT.0. .OR. hmix.LT.0. OR.
     %   zr.LT.0. .OR. swr.LT.0.) then
                                  sigma_v = -9999.
                                  return
      endif

      if(zr.gt.0. .AND. swr.gt.0.) then
                                   swrc    = SIGMAW (zr,us,ws,hmix)
                                   f       = swr/swrc
                                   SIGMA_W = f * SIGMAW (z,us,ws,hmix)
      else
                                   SIGMA_W = SIGMAW (z,us,ws,hmix)
      endif

      return
      end

c     =================================================================
      Function SIGMAW (z,us,ws,hmix)
c     =================================================================
      data c1,c2,c3/1.54,1.457,1.207/
      if(ws.gt.0.) then
c                  convective situation
                   s1 = c1 * ws*ws * (z/hmix)**.666667
                   s1 = s1 * exp(-2.*z/hmix)
                   if(z.lt.hmix) then
                              s2 = c2 * us*us * (1.-z/hmix)*(1.-z/hmix)
                   else
                              s2 = 0.
                   endif
c
                   sigmaw = sqrt(s1+s2)
      else
c
c                  stable situation
                   if(z.lt.0.9*hmix) then
                                     sigmaw = c3 * us * (1.-z/hmix)
                   else
                                     sigmaw = 0.1207*us
                   endif
      endif
c
      return
      end


c&@   =================================================================
      Function TSTAR_0(rc,us,h0)
c     =================================================================
*     This routine returns the value of the scale temperature starting
*     from the friction velocity and the sensible heat flux.
*
*     Input data:
*                rc  = density * specific heat
*                us  = friction velocity (m/s)
*                h0  = sensible heat flux (W/m2)
*
*     Output data:
*                TSTAR_0 = scale temperature (Kelvin)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      if(rc.LT.0. .OR. us.LT.0.) then
                                 TSTAR_0 = -9999.
                                 return
      endif

      TSTAR_0 = -h0 / (rc*us)

      return
      end
c
c&@   =================================================================
      Function TSTAR_1(zt1,zt2,dtet,hlm)
c     =================================================================
*     This routine computes the scale tempeature starting from the po-
*     tential temperature difference between zt1 and zt2 (zt2>zt1).
*
*     Input data:
*                zt1 = lower height where potential temp.is measured (m)
*                zt2 = upper height where potential temp.is measured (m)
*                dtet= potential temperature difference between zt2)
*                      and zt1 (Kelvin)
*                hlm = 1/Monin Obukhov length (1/m)
*
*     Output data:
*                TSTAR_1 = scale temperature (Kelvin)
*                          -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data hk/0.4/

      tstar_1=-9999.
      if(zt1.GT.zt2) return

      alt = alog(zt2/zt1)

      if(hlm.LT.0.) then
c                   convective situation
                    psi1 = PSIH_C(zt1,hlm)
                    psi2 = PSIH_C(zt2,hlm)
                           if(psi1.EQ.-9999.) return
                           if(psi2.EQ.-9999.) return
      else
c                   stable situation
                    psi1 = PSIH_S(zt1,hlm)
                    psi2 = PSIH_S(zt2,hlm)
                           if(psi1.EQ.-9999.) return
                           if(psi2.EQ.-9999.) return
      endif

      TSTAR_1 = hk * dtet / (alt-psi2+psi1)

      return
      end

c&@   =================================================================
      Function USTAR_0(z0,zr,hlm,vel)
c     =================================================================
*     This routine returns the value of the friction velocity computed
*     from basic definition.
*
*     Input data:
*                z0   = roughness length (m)
*                zr   = height where wind speed is measured (m)
*                hlm  = 1/Monin Obukhov length (1/m)
*                vel  = average wind speed at zr  (m/s)
*
*     Output data:
*                USTAR_0 = friction velocity (m/s)
*                          (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      DATA hk/0.4/

      ustar_0=-9999.
      if(z0.LT.0. .OR.zr.LT.0. .OR. zr.LT.z0 .OR.
     %   vel.LT.0.) return

      if(hlm.LT.0.) then
                    psim = PSIM_C(zr,hlm)
      elseif(hlm.GT.0.) then
                    psim = PSIM_S(zr,hlm)
      else
                    psim = 0.
      endif
      if(psim.EQ.-9999.) return

      USTAR_0 = hk*vel/(alog(zr/z0)-psim)

      return
      end

c&@   =================================================================
      Function USTAR_1(zu1,zu2,du,hlm)
c     =================================================================
*     This routine computes the friction velocity starting from the
*     wind measurements at zu1 and zu2 levels (zu2>zu1).
*
*     Input data:
*                zu1  = lower height where wind speed is measured (m)
*                zu2  = upper height where wind speed is measured (m)
*                du   = vel(zu2) - vel(zu1) (m/s)
*                hlm  = 1/Monin Obukhov length (1/m)
*
*     Output data:
*                USTAR_1 = friction velocity (m/s)
*                          -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data hk/0.4/

      USTAR_1=-9999.
      if(zu1.GT.zu2 .OR. du.LT.0.) return

      alu = alog(zu2/zu1)

      if(hlm.LT.0.) then
c                   convective situation
                    psi1 = PSIM_C(zu1,hlm)
                    psi2 = PSIM_C(zu2,hlm)
                           if(psi1.eq.-9999.) return
                           if(psi2.eq.-9999.) return
      else
c                   stable situation
                    psi1 = PSIM_S(zu1,hlm)
                    psi2 = PSIM_S(zu2,hlm)
                           if(psi1.eq.-9999.) return
                           if(psi2.eq.-9999.) return
      endif

      USTAR_1 = hk * du / (alu-psi2+psi1)

      return
      end

C&@   ==================================================================
      Function W_STAR(T,h0,hmix)
C     ==================================================================
*     This routine computes the free convection scaling velocity.
*
*     Input:
*             T    = air temperature (øK)
*             h0   = turbulent sensible heat flux (W/mq)
*             hmix = mixing height (m)
*
*     Output:
*             w_star = free convection scaling velocity (m/s)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data g/9.81/

      wstar=-9999.
      if(T.LT.0. .OR. hmix.LT.0.) return
c
      if(h0.le.0.) then
c                  Stable condition
                   w_star = 0.
                   return
      endif
c
      rc     = RHOCP(T)
      if(rc .EQ. -9999.) return

      w_star = (g * hmix * h0 / (rc * T))**0.33333
c
      return
      end

c& @  =================================================================
      Function ZLH_C(psih)
c     =================================================================
*     The function returns the value of the stability parameter z/L (ratio
*     between the reference height and the Monin-Obukhov length) star-
*     ting from the value of universal heat function in convective
*     situations.
*
*     Input data:
*                iaut = 1 -> Businger relation (DEFAULT)
*        NOTE! iaut must be set up using set_author_pbl routine.
*
*                psih = universal heat function
*
*     Output data:
*                ZLH_C = ratio between the reference height and the
*                        Monin-Obukhov length in convective conditions.
*                        (-9999. if error)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set

      ZLH_C=-9999.
      if(psih.LT.0.) return

      if(ihc_set.ne.1) then
               iaut=1
      else
               iaut=ihc_set
      endif

      go to (1) iaut

c ->     Businger relation
 1       y     = 2.*exp(psih/2.) - 1.
         ZLH_C = (1.-y*y)/16.
         return
      end

c& @  =================================================================
      Function ZLH_S(psih)
c     =================================================================
*     This routine returns the value of stability parameter zl (ratio
*     between the reference height and the Monin-Obukhov length) star-
*     ting from the value of universal heat function in stable
*     situations.
*
*     Input data:
*                iaut = 1 -> Businger relation
*                       2 -> van Ulden Holtslag relation (DEFAULT)
*                       3 -> Beljaars Holtslag relation
*        NOTE! iaut must be set up using set_author_pbl routine.
*
*                psih = universal momentum function
*
*     Output data:
*                ZLH_S = ratio between the reference height and the
*                        Monin-Obukhov length in stable conditions.(m)
*                        -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      real b1(5),b2(5)
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set
      DATA b1/-1.999152E-01,5.203818E-03,-1.217037E-04,1.940718E-06,
     %        2.885323E-07/
      DATA b2/-2.047663E-01,2.846221E-03,-5.246553E-04,-2.573392E-05,
     %        -3.333224E-07/
c    ..................................................................
      ZLH_S=-9999.
      if(psih.GT.0.) return

      if(ihs_set.ne.1  .or.
     %   ihs_set.ne.2  .or.
     %   ihs_set.ne.3) then
               iaut=2
      else
               iaut=ihs_set
      endif

      go to (1,2,3) iaut

c ->     Businger relation
 1       ZLH_S = -psih/5.
         return

c ->     van Ulden Holtslag relation
 2       ZLH_S = -alog(1.+psih/17.)/0.29
         return

c ->     Beljars Holtslag relation
 3       if(psih.LT.-29.6703) then
                              ZLH_S = 10.
                              return
         endif
         if(psih.GE.-4.43559) then
                              a = 0.
                              do i=1,5
                                   a = (b1(6-i)+a)*psih
                              enddo
         else
                              a = 0.
                              do i=1,5
                                   a = (b2(6-i)+a)*psih
                              enddo
         endif
         ZLH_S = a
         return
      end

c& @  =================================================================
      Function ZLM_C(psim)
c     =================================================================
*     This routine returns the value of stability parameter zl (ratio
*     between the reference height and the Monin-Obukhov length) star-
*     ting from the value of universal momentum function in convective
*     situations.
*
*     Input data:
*                iaut = 1 -> Businger relation (DEFAULT)
*                       2 -> van Ulden Holtslag relation
*                       3 -> Carl relation
*        NOTE! iaut must be set up using set_author_pbl routine.
*
*                psim = universal momentum function
*
*     Output data:
*                ZLM_C = ratio between the reference height and the
*                        Monin-Obukhov length in convective conditions.
*                        (m)
*                        -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      real b1(5),b2(5)
      real b3(5),b4(5)
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set

      DATA b1/-2.449843E-01,-3.412069E-01,-9.557329E-02,-1.083350E-01,
     %         0.000000E+00/
      DATA b2/-3.087940E-01,-8.881401E-02,-4.663084E-01,1.346109E-01,
     %         -6.125474E-02/
      DATA b3/-1.812528E-01,-2.159515E-01,-3.571875E-02,-5.755772E-02,
     %         0.000000E+00/
      DATA b4/-2.660852E-01,5.824984E-02,-3.611408E-01,1.130560E-01,
     %        -3.409206E-02/
c    ..................................................................
      ZLM_C=-9999.
      if(psim.LT.0.) return

      if(imc_set.ne.1  .or.
     %   imc_set.ne.2  .or.
     %   imc_set.ne.3) then
               iaut=1
      else
               iaut=imc_set
      endif

      go to (1,2,3) iaut

c ->     Businger relation
 1       if(psim.GT.2.5493) then
                            ZLM_C = -10.
                            return
         endif
         if(psim.LE.1.1162) then
                            a = 0.
                            do i=1,5
                                 a = (b1(6-i)+a)*psim
                            enddo
         else
                            a = 0.
                            do i=1,5
                                 a = (b2(6-i)+a)*psim
                            enddo
         endif
         ZLM_C = a
         return

c ->     van Ulden Holtslag relation
 2       a     = 1.+psim
         a     = a*a
         a     = a*a
         ZLM_C = (1. - a)/16.
         return

c ->     Carl relation
 3       if(psim.GT.3.0720) then
                            ZLM_C = -10.
                            return
         endif
         if(psim.LE.1.40208) then
                            a = 0.
                            do i=1,5
                                 a = (b3(6-i)+a)*psim
                            enddo
         else
                            a = 0.
                            do i=1,5
                                 a = (b4(6-i)+a)*psim
                            enddo
         endif
         ZLM_C = a
         return

      end

c& @  =================================================================
      Function ZLM_S(psim)
c     =================================================================
*     This routine returns the value of stability parameter zl (ratio
*     between the reference height and the Monin-Obukhov length) star-
*     ting from the value of universal momentum function in stable
*     situations.
*
*     Input data:
*                iaut = 1 -> Businger relation
*                       2 -> van Ulden Holtslag relation (DEFAULT)
*                       3 -> Beljaars Holtslag relation
*        NOTE! iaut must be set up using set_author_pbl routine.
*
*                psim = universal momentum function
*
*     Output data:
*                ZLM_S = ratio between the reference height and the
*                        Monin-Obukhov length in stable conditions.(m)
*                        -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      real b1(5),b2(5)
      common/set_authors/ imc_set,ims_set,ihc_set,ihs_set

      DATA b1/-1.923147E-01,6.521917E-03,-2.855482E-04,9.194954E-06,
     %        -1.089428E-06/
      DATA b2/-1.367452E-01,3.337318E-02,4.444154E-03,3.498340E-04,
     %         7.170634E-06/
c    ..................................................................
      ZLM_S=-9999.
      if(psim.GT.0.) return

      if(ims_set.ne.1  .or.
     %   ims_set.ne.2  .or.
     %   ims_set.ne.3) then
               iaut=2
      else
               iaut=ims_set
      endif

      go to (1,2,3) iaut

c ->     Businger relation
 1       ZLM_S = -psim/5.
         return

c ->     van Ulden Holtslag relation
 2       ZLM_S = -alog(1.+psim/17.)/0.29
         return

c ->     Beljars Holtslag relation
 3       if(psim.LT.-17.6172) then
                              ZLM_S = 10.
                              return
         endif
         if(psim.GE.-4.39257) then
                              a = 0.
                              do i=1,5
                                   a = (b1(6-i)+a)*psim
                              enddo
         else
                              a = 0.
                              do i=1,5
                                   a = (b2(6-i)+a)*psim
                              enddo
         endif
         ZLM_S = a
         return

      end

C&@   ==================================================================
      Subroutine PBL_0(z0,cloud,T,Rg,zr,vel,  us,ts,h0,hlm)
C @   ==================================================================
*     This routine computes the Surface Layer parameters starting from
*     basic meteorological informations.
*     The calculation method is as proposed by:
*     - Maul (1980) for sensible heat flux,
*     - Wang and Chen (1980) for friction velocity in convectivity,
*     - Venkatram (1980) for stability parameterization.
*     Note: this method is the one used in MESOPUFF-II.
*
*     Input:
*              z0    = roughness length (m)
*              cloud = total cloud cover (0..1)
*              T     = air temperature (Kelvin deg.)
*              Rg    = incoming global radiation (W/m**2)
*              zr    = height where wind speed is measured (m)
*              vel   = average wind speed at zr  (m/s)
*
*     Output:
*                us   = friction velocity (m/s)
*                ts   = scale temperature (Kelvin)
*                hlm  = 1/Monin Obukhov length (1/m)
*                h0   = sensible heat flux (W/m2)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      data alfa/0.3/,hk/0.4/, gamma/4.7/, aa/1100./
      hlm=-9999.
      us =-9999.
      h0 =-9999.
      ts =-9999.

      if(rg    .eq. -9999.) return
      if(cloud .eq. -9999.) return
      if(vel   .le.  0.   ) return
      if(z0    .le.  0.   ) return
      if(T     .le.  200. ) return
      if(zr    .le.  4.*z0) return

c     rhocp is computed
      rc = RHOCP(T)
      if(rc.EQ.-9999.) return

c     sensible heat flux is computed
      h0= alfa * Rg + 24.*cloud - 25.5
      q0=h0/rc

      zm=zr- 4.*z0
      alzm=alog(zm/z0)
      if(h0.gt.0.) then
c       convective
        us$= hk*vel/alzm
        q0$= T*us$**3./(hk*9.81*zm)
        if( z0/zm .le. 0.01) then
          a= 0.128+0.005*alzm
        else
          a= 0.107
        endif
        b= 1.95+32.6* (z0/zm)**0.45
        us= us$*(1.+a*alog(1.+b*q0/q0$))
        hlm = -(9.81*hk*q0)/(us**3*T)
        ts= - q0/us
      else
c       stable
        cdn= hk/alzm
        u0 = gamma * zm/(hk*aa)
        c= 1.- 4.*u0/(cdn*vel*vel)
        if(c.lt.0.) c=0.

        us= 0.5*cdn*vel*(1.+c**0.5)
        h0= -rc*us*T/(9.81*hk*aa)
        hlm=1./(aa*us*us)
        ts= -h0/(rc*us)
      endif

      return
      end

C&@   ================================================================
      Subroutine PBL_3A (z0,zr,vel,T,h0,  us,ts,hlm)
C     ================================================================
*     This routine computes the SL parameters by a wind measure at zu
*     level and h0 (sensible heat flux).
*     Note: This routine is very sensible to the h0 value, mainly
*           during nigh-time hours. Be sure of the goodness of h0 value.
*
*     Input data:
*              z0    = roughness length (m)
*              zr    = height where wind speed is measured (m)
*              vel   = wind speed at zr  (m/s)
*              T     = mean air temperature (Kelvin)
*              h0    = sensible heat flux (W/m2)
*
*     Output data:
*                us   = friction velocity (m/s)
*                ts   = scale temperature (Kelvin)
*                hlm  = 1/Monin Obukhov length (1/m)
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     -------------------------------------------------------------------
      data hk/0.4/

      us  = -9999.
      ts  = -9999.
      hlm = -9999.

      if(z0.LT.0. .OR. zr.LT.z0 .OR. vel.LT.0. .OR. T.LT.0.) return

c     rhocp is computed
      rc = RHOCP(T)
      if(rc .EQ.-9999.) return

c     first guess for us
      alu = alog(zr/z0)
      us  = hk * vel / alu
      hlm_= HLM_2(rc,T,us,h0)
      if(hlm.eq.-9999.) return
c
c     iterative procedure
      do i=1,20
         us  = USTAR_1(z0,zr,vel,hlm_)
         if(us.eq.-9999.) return
         hlm = HLM_2(rc,T,us,h0)
         if(hlm .EQ.-9999.) return
         if(abs((hlm_-hlm)/hlm_).lt.0.01) exit
         hlm_=hlm
       enddo
       ts  = TSTAR_0(rc,us,h0)
       if(ts.eq.-9999.) return

      return
      end
c&    =================================================================
      Subroutine Wind_scalar(n_dati,u,d,vel,dir,su,st)
c     =================================================================
*     This routine computes the horizontal average wind speed and direc-
*     tion and relative standard deviations starting from measuraments
*     of conventional anemometer.
*
*     Input: n_dati = number of data
*            u      = array of wind speeds (m/s)
*            d      = array of wind directions (degrees)
*
*     Output: vel   = average wind speed (m/s)
*             dir   = average wind direction (degrees)
*             su    = standard deviation of wind speed (m/s)
*             st    = standard deviation of wind direction (degrees)
*
*     WARNING!
*     If number of valid data is less than n_dati/2
*             vel = 0.
*             dir = -9999.
*             su  = -9999.
*             st  = -9999.
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     -----------------------------------------------------------------
      REAL u(n_dati),d(n_dati)
      data rad/57.29578/
c
c     azzeramenti delle sommatorie
      num = 0
      vel = 0.
      s_sin = 0.
      s_cos = 0.
      s_sin_x=0.
      s_cos_y=0.
      sum_u = 0.
c
      do i=1,n_dati
           if(u(i).gt.0.) then
                           num   = num +1
                           vel   = vel + u(i)
                           ddir  = d(i)/rad
                           sind= sin(ddir)
                           cosd= cos(ddir)
                           s_sin = s_sin + sind
                           s_cos = s_cos + cosd
                           sum_u = sum_u + u(i)*u(i)
                           s_sin_x = s_sin_x + u(i)*sind
                           s_cos_y = s_cos_y + u(i)*cosd
           endif
      enddo
c
      if(num.GE.n_dati/2.) then
                           num=n_dati
                           s_sin = s_sin/float(num)
                           s_cos = s_cos/float(num)
                           s_sin_x = s_sin_x/float(num)
                           s_cos_y = s_cos_y/float(num)
c
                           vel = vel/float(num)
c                          protection against both values=0
                           if(s_sin_x.eq.0. .and. s_cos_y.eq.0.) then
                             vel   = 0.
                             dir   = -9999.
                             su    = -9999.
                             st    = -9999.
                             return
                           else
                             dir = ATAN2(s_sin_x,s_cos_y)*rad
                           endif
                           if(dir.lt.0.) dir=dir+360.
                           su  = SQRT(sum_u/num) - vel*vel
                           sss= 1. -(s_sin*s_sin+s_cos*s_cos)
                           if(sss.le.0.) then
                              st=0.
                           else
                             epsil = sqrt(sss)
                             st  = asin(epsil)*(1.+0.1547*epsil**3)*rad
                           endif
      else
                           vel   = 0.
                           dir   = -9999.
                           su    = -9999.
                           st    = -9999.
      endif
c
      return
      end
c
c
c&    =================================================================
      Subroutine MED_VAR(n_dati,x,xm,sx)
c     =================================================================
*     Computes the mean value and standard deviation of the x array
*     No cheks on data.
*
*     Input: n_dati = number of x data
*            x      = x array of data
*
*     Output: xm  = mean of x array
*             sx  = standard deviation of x array
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      Real x(n_dati)

      xm  = 0.
      sx2 = 0.
      do i=1,n_dati
         xm  = xm + x(i)
      enddo

      if(n_dati.LE.0) then
                   xm  = -9999.
                   sx  = -9999.
      else
                   xm = xm/n_dati
                   do i=1,n_dati
                      sx2 = sx2 + (x(i)-xm)**2
                   enddo
                   sx = sqrt(sx2/n_dati)
      endif
      return
      end
c&    =================================================================
      Subroutine WIND_VECTOR(n_dati,vx,vy,vz,vel,dir,wm,su,st,sw)
c     =================================================================
*     This routine returns the horizontal mean wind and direction and
*     the relative standard deviations. Moreover returns the mean ver-
*     tical mean speed and standard deviation.
*
*     Input: n_dati = number of data
*            vx     = x wind component (m/s)
*            vy     = y wind component (m/s)
*            vz     = z wind component (m/s)
*
*     Output: vel = horizontal mean wind speed (m/s)
*             dir = horizontal wind direction (degrees centigrade)
*             wm  = vertical mean wind speed (m/s)
*             su  = standard deviation of the wind speed (m/s)
*             st  = standard deviation of wind direction (degrees)
*             sw  = standard deviation of vertical wind speed (m/s)
*
*     If the number of the valid data if less than n_dati/2
*            vel = 0.
*            dir = -9999.
*            su  = -9999.
*            st  = -9999.
*            wm  =  0.
*            sw  = -9999.
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      REAL vx(n_dati),vy(n_dati),vz(n_dati)
      data rad/57.29578/
c
      vel   = 0
      vxm   = 0.
      vym   = 0.
      s_sin = 0.
      s_cos = 0.
c
      do i=1,n_dati
           if(abs(vx(i)).ne.0. .OR.  abs(vy(i)).ne.0.) then
                                           num   = num +1
                                           vxm   = vxm + vx(i)
                                           vym   = vym + vy(i)
                                           dir   = dir_wind(vx(i),vy(i))
                                           s_sin = s_sin + sin(dir/rad)
                                           s_cos = s_cos + cos(dir/rad)
           endif
      enddo

      if(num.le. n_dati/2) then
                    vel   = 0.
                    dir   = -9999.
                    su    = -9999.
                    st    = -9999.
      else
                    vxm   = vxm/n_dati
                    vym   = vym/n_dati
                    s_sin = s_sin/n_dati
                    s_cos = s_cos/n_dati
                    vel   = sqrt(vxm*vxm+vym*vym)
                    dir   = dir_wind(vxm,vym)
c
                    epsil = sqrt(1. -(s_sin*s_sin + s_cos*s_cos))
                    st    = asin(epsil)*(1.+0.1547*epsil*epsil*epsil)
                    st    = st*rad
c
                    sum_u = 0.
                    do i=1,n_dati
                     if(abs(vx(i)).gt.0. .AND. abs(vy(i)).gt.0.)
     %                  sum_u = sum_u + (sqrt(vx(i)**2+vy(i)**2)-vel)**2
                    enddo
                    sum_u = sum_u/n_dati
                    su    = sqrt(sum_u)
      endif

      num   = 0
      wm    = 0.
      sum_w = 0.
      do i=1,n_dati
           if(abs(vz(i)).gt.0.) then
                                num   = num+1
                                wm    = wm  + vz(i)
                                sum_w = sum_w + vz(i)*vz(i)
           endif
      enddo
      if(num.le.n_dati/2) then
                    wm    = 0.
                    sw    = -9999.
      else
      wm   = wm /n_dati
      sw   = sqrt(sum_w/n_dati - wm*wm)
      endif

      return
      end


c&    =================================================================
      Function DIR_WIND(vx,vy)
c     =================================================================
*     This routine computes the wind direction starting from vx
*     (East-West) and the vy (North-South) component.
*     Input: vx = East-West component, positive if from East(m/s)
*            vy = North-South component, positive if from North (m/s)
*
*     Output: DIR_WIND = direction from which the wind is blowing
*                        -8888 if wind calme
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      if(vx.eq.0..and.vy.eq.0.) then
                                dir_wind = -8888.
                                return
      endif
c
      dir_wind = atan2(vx,vy)*57.29578
      if(dir_wind.lt.0.  ) dir_wind = dir_wind + 360.
      if(dir_wind.gt.360.) dir_wind = dir_wind - 360.
c
      return
      end
c

c&    =================================================================
      Function z0_epa(z,u,su)
c     =================================================================
*     This routine returns the z0 value according to EPA method.
*
*     Input: z   = reference height (m)
*            vel = average wind speed (m/s)
*            su  = standard deviation of wind speed (m/s)
*
*     Ourput: z0_epa = roughness length (m)
*                      -9999. if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      z0_epa=-9999.
      if(u.le.0. .AND. su.le.0.) return
c
      z0_epa = z * exp(-u/su)
c
      return
      end
c
c
c& @  =================================================================
      Function j_day(ia,im,ig)
c     =================================================================
*     This rotine determines the Julian day starting from current date
*
*     Input:
*             ia    =  year
*             im    =  month
*             ig    =  day
*
*     Output:
*             j_day = Julian day
*                       -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      Integer ngm(13,2), ngmax(12,2)
c
      DATA ngm/0,31,60,91,121,152,182,213,244,274,305,335,366,
     $         0,31,59,90,120,151,181,212,243,273,304,334,365/
      DATA ngmax/31,28,31,30,31,30,31,31,30,31,30,31,
     $           31,29,31,30,31,30,31,31,30,31,30,31/

      j_day = -99
      if(ia.LE.0 .OR. im.LE.0 .OR. im.GT.12 .OR. ig.LE.0 ) return

      ibis= MOD(ia,4)
      if(ibis .ne. 0 ) then
        if(ig .gt. ngmax(im,1)) return
      else
        if(ig.gt. ngmax(im,2)) return
      endif
      if(ibis .ne. 0 ) then
                      j_day = ig+ngm(im,1)
      else
                      j_day = ig+ngm(im,2)
      endif
c
      return
      end

c& @  =================================================================
      INTEGER FUNCTION CLASS_WIND(n,v_up,vel)
c  @  =================================================================
*     Defines wind speed  class (from 1 to n) starting from a given
*     number of reference levels (v_up(n-1) array).
*
*     Input:
*      n    = total number of wind speed classes
*      v_up = is an n-1 dimension array. Infact N-th class is considered
*             in the case of wind speed> v_up(n-1)
*      vel  = wind speed (m/s)
*
*     Note: The v_up array must be declared and inizilaized at the
*           required reference levels.
*
*     Output:
*            Class_wind = wind speed class
*                         -99 if error
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      real v_up(n-1)
      data wind_max/99./

      if(n.lt.1 .OR. v_up(n-1).le.0. .OR. vel.lt.0.
     %   .OR. vel.gt.wind_max) then
                               class_wind = -99
                               return
      endif

      do iv=1,n-1
           if(vel.le. v_up(iv)) then
                                class_wind = iv
                                return
           endif
      enddo

      class_wind = n
      return
      end
c
c& @  =================================================================
      INTEGER FUNCTION CLASS_DIR (iflag,n,dir)
c  @  =================================================================
*     Defines wind direction class (from 1 to n) considerind two
*     possibility:
*
*     Input:
*        iflag = 0 directions are centered on Cardinal Directions
*                1 directions are considered as Italian Air Force
*                  (not centered on Cardinal Directions)
*        n     = number of directions (from 1 to 360)
*        dir   = wind direction (degree centigrade)
*
*     Output: CLASS_DIR = wind direction class
*                         -99 if error
*
c     By:   Servizi Territorio coop a r.l. (17 March 1993)
c     -----------------------------------------------------------------
      class_dir=-99
      if(n.lt.1 .or. n.gt.360) return

      if(iflag.ne.0 .and. iflag.ne.1) return

      if(dir.lt.0. .or. dir.gt.360.) return

      teta=360./float(n)

      if(iflag.eq.0)  then
                      alfa = teta/2.
      else
                      alfa=0.
      endif

      class_dir = 1
      do jd=1,n
           c_dir= jd * teta - alfa
           if(dir.le.c_dir) then
                            class_dir=jd
                            return
           endif
      enddo
      return
      end

c&    =================================================================
      Subroutine DAY_J(jday,ia,ig,im)
c     =================================================================
*     This routine determines the current date from Julian day
*
*     Input:
*             jday    = Julian day
*             ia      = current year
*
*     Output:
*             im    =  month
*             ig    =  day
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      INTEGER ngm(13,2)
c
      data ngm/0,31,60,91,121,152,182,213,244,274,305,335,366,
     $         0,31,59,90,120,151,181,212,243,273,304,334,365/
c
      im=-99
      ig=-99
      if(jday.LE.0 .OR. jday.GT.366) return

      if(jday.gt.0) then
                    ianno = ia/4*4
                    iab   = 2
                    if(ianno.eq.ia) iab=1
                    nn=0
                    do i=1,13
                         if(jday.le.ngm(i,iab)) then
                                                im = i-1
                                                ig = jday-ngm(i-1,iab)
                                                return
                         endif
                    enddo
      endif
      return
      end
c
C&    ==================================================================
      Subroutine GILL_cor(VX,VY,VZ)
C     ==================================================================
*     This subroutine performs the correction of data from Gill propeller
*     anemometer (GILL 08274)
*
*     VX wind component along X axis (m/sec).
*     VY wind component along Y axis (m/sec).
*     VZ wind component along Z axis (m/sec).
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      REAL GILL(101)
      DATA GILL /1.04,5*1.05,1.06,2*1.07,1.08,1.09,1.1,1.11,1.12,1.13
     *,1.15,1.16,1.18,1.19,1.21,1.23,1.24,2*1.25,3*1.26,3*1.25,1.26,1.27
     *,1.28,3*1.29,1.28,1.29,1.3,1.29,1.27,1.24,1.22,1.19,1.16,1.12,1.06
     *,1.,.94,.96,2*1.,1.09,1.46,1.62,1.55,1.52,3*1.51,3*1.5,1.49,1.47,
     *1.46,1.48,1.5,1.49,1.47,1.44,1.42,1.4,1.37,1.34,1.32,1.31,1.3,1.29
     *,1.28,1.27,1.26,1.25,1.22,1.2,1.18,1.16,1.14,1.12,1.1,1.09,1.08
     *,1.07,1.06,1.05,1.04,1.03,2*1.02,1.01,1./
C
      N=0
      IX=1
      IY=1
      IZ=1
      VXX=VX
      VYY=VY
      VZZ=VZ
C
 77   IF(N.EQ.6) GOTO 99
      VX=VXX*GILL(IX)
      VY=VYY*GILL(IY)
      VZ=VZZ*GILL(IZ)
      if(vx.eq.0. .and. vy.eq.0. .and. vz.eq.0.) return
      SMOD=(VX**2+VY**2+VZ**2)**.5
      IIX=INT(VX*50./SMOD)+51
      IIY=INT(VY*50./SMOD)+51
      IIZ=INT(VZ*50./SMOD)+51
      IF((ABS(IIX-IX).GT.2).OR.(ABS(IIY-IY).GT.2).OR.(ABS(IIZ-IZ)
     *.GT.2)) THEN
         N=N+1
         IX=IIX
         IY=IIY
         IZ=IIZ
         GOTO 77
      ELSE
         RETURN
      ENDIF
 99   CONTINUE
      RETURN
      END
C

c&    =================================================================
      Character*2 Function num2$(n)
c     =================================================================
*     This routine converts the integer number n into a 2 character string
*     It is commonly used in defining names of files to be created and/or
*     opened.
*
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      i1 = n/10
      i2 = n-i1*10

      num2$ = CHAR(48+i1)//CHAR(48+i2)

      return
      end

c&    =================================================================
      Character*3 Function  num3$(n)
c     =================================================================
*     This routine converts the integer number n into a 3 character string
*     It is commonly used in defining names of files to be created and/or
*     opened.
*
c
c     Servizi Territorio coop a r.l.- Cinisello B. (MI) Italy - 1993
c     ------------------------------------------------------------------
      i1 = n/100
      i2 = (n-i1*100)/10
      i3 = n-i1*100-i2*10

      num3$ = CHAR(48+i1)//CHAR(48+i2)//CHAR(48+i3)

      return
      end

