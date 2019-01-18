# The "Air LAgrangian particle MOdel" (ALAMO)

Since its inception, as an experimental project within Servizi Territorio srl (circa 2002), the ALAMO particle model, written by prof. Roberto Sozzi, has found s rtong root in the PBL_MET.

The newest version, whose code you may find in the "/example" project directory, has been largely refactored to exploit the new characteristics of modern Fortran, but still retains a heavy foothold on the new _pbl_met_.

The vice-versa is even true: part of the ALAMO legacy code, namely the routines devoted to guessing reasonable vertical profiles for some surface-measured meteorological parameters, has been polished, maquillaged, and moved to the "pbl_simil.f90" module of the new _pbl_met_: we may say, co-evolution at work.

And, the story is continuing...
