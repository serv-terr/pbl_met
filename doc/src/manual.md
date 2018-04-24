![pbl_met_logo](./pbl_met_Logo.png)

# Short description of _pbl_met_


## What is it?

_pbl_met_ is a library composed by various Fortran modules, test programs and accompanying data. The purpose of _pbl_met_ is to alleviate the chore of writing meteorological processors and data processing systems, by providing routines computing or estimating the quantities commonly required by atmospheric dispersion models and other computing codes.


## Why open source?

By its very nature, the _pbl_met_ plays a foundational role in open source and commercial applications (for example the SODAR-aware meteorological processor and atmospheric dispersion input preparation code _ST-Me_ by Servizi Territorio srl).

Because of this foundational role, it is important its structure and implementation details are accessible to everyone for inspection, correction and extension. This can happen if the _pbl_met_ code is open source, and people actually access and peruse it.


## Library organization

_pbl_met_ is delivered as a set of Fortran modules, each dedicated to a specific theme in met processing (e.g. psychrometry, radiation, PBL, ...).

Individual modules are interdependent, and then it is advisable to always download the whole library.

In this Wiki individual modules are also shortly described: check index to discover all about them. Also please consider _pbl_met_ has been intentionally designed for source readability: sources themselves are the eventual documentation. _pbl_met_ is open source, so please consider exploiting this nice opportunity.


## Fortran version and reference compilers

Modules are written in Fortran 2003 with a minor use of 2008 extensions. Most _pbl_met_ modules however comply with the simpler Fortran 95.

_pbl_met_ has been designed to be compiled using the GNU Fortran Compiler, [`gfortran`](https://gcc.gnu.org/fortran/). Compilation has been tested from `gfortran` version 4.4 and following. If you use an older version we recommend you upgrade your compiler.

Also notice the authors and maintainers of _pbl_met_ will make no specific effort to held code compatible with verson 4.4 of `gfortran` - nor to make specific effortsto breach it intentonally, to the extent possible. That said, we would like to reassure you about the fact _pbl_met_ has a relatively simple structure, being mostly composed by neat functions with ew defined inputs and, in most case, a unique output.


## Support of compilers other than `gfortran`

In addition to the GNU Fortran, we support use of the Intel Fortran Compiler. The community edition of it has been used in Linux for this open-source project.

Some of the authors have successfully built _pbl_met_ with compilers from other developers/vendors. Our current impression is _pbl_met_ is quite compiler independent, but our tight time frame did not allow for a systematic test. By the way, this is an interesting volunteering area.


## License

_pbl_met_ is released under LGPL v3.0.

As such, it may be freely incorporated in both open and closed source codes without a need for explicit endorsement by Servizi Territorio srl. If you have not already done, we advise you to check carefully at [GNU Foundation](http://www.gnu.org/licenses/lgpl-3.0.html) site to discover any restriction which applies.

We'll be glad to get from you, in case you employ _pbl_met_, to let us know. We'd also appreciate your citation, in case you have used _pbl_met_ to process data for a document to be published somewhere (including of course papers on peer-reviewed journals).


## Intended use of modules

We, the authors, include _pbl_met_ modules directly in our projects, whenever we need them, directly in source form.

Unlike in the old PBL_MET, we have intentionally and positively decided to _not_ package _pbl_met_ as anobject library.

We feel using sources directly, rather than linking to a pre-canned object library, encourages curiosity: meteorological processing is quite an art more than science, and a critical mind set is essential to produce intellectually honest and professionally sound results. This is an easy catch given _pbl_met_ structural simplicity - really, building a library seemed us overkill.

Of course you are free to package together _pbl_met_ modules as an object library if you like. We too have made from time to time. Using sources directly instead of compiled object code is quite the norm in communities like Ada language users, as a way to encourage code understanding - something of great value in safety-critical applications. The same is maybe not yet so common among Fortran programmers, but we would like you too give this way a try.


## Coding style

We've choosen to privilege readability and understandability over extreme code optimization. After all, what we consider "processing a huge mass of meteorological data" means dealing with thousands, or tens of thousands, maybe a million data records: an almost-nothing by today computing standards. The meaning of "efficiency" is really not the same it was some years ago.

On the other side, some of the meteorological data processing or estimation is quite intricate, and not always consensus has set over one method or the other. So, being able to understand what is going on behind the hood may be welcome.

By reading the sources, you may get an impression of a "high-oxytocin-low-steroid" place. That is. Maybe, gathering something orderly and useful from a mess of data is a task more eliciting the _yin_ rather than the _yang_ side.

Nevertheless, the fact this task occurs most often invisibly to final users does not means it is extremely fascinating on one side, but also in the meanwhile, extraordinarily dangerous if done haphazardly and without placing love and responsibility in the process.

More specifically, we have done our best to clarify our intentions about routine argument use, formulae, and so on. We intentionally have used long, meaningful names (unless some internationally known symbol is in wide use to mean the same thing). We've employed extensively the syntax features allowed by Fortran 95 and following to produce clear code, with some (sparingly) use of newer Fortran 2003 and 2008 constructs, where useful.

From time to time we had to use some "applied-math stuff" like solving equations iteratively, or finding solutions to ordinary differential equations, and so on: whenever possible we included the due code directly in the modules using them, as internal routines,so you may easily figure out what is happening.

We understand Fortran modules, used carelessly, may make their users life miserable (either by a horrible mess of overlapping names, or the even worse prefixing used to avoid them. To prevent thistrouble source we defined the default visibility of module symbols `PRIVATE`, thus exporting the very few important as `PUBLIC`. This will help you with name overlaps.

A systematic use of `IMPLICIT NONE` statement has also been used. By so doing we actually constrict ourselves to declare all variables, a very healthy habit some old-style-high-steroid FORTRAN programmer might not completely appreciate. We apologise to them all - but acknowledge the priority is in delivering code which is correct, or (better even) may be proven to be.

We made our best to write code you can understand (possibly, with a bit of study and application - the subject is intrinsically quite advanced physics - say, not the type you find in high school). But we can't ensure having made all things "ideally". Sorry so sloppy. If in case, anyway, please do not hesitate to contact us - our e-mail-office-door is tendentially open (if we do not respond within a bunch of nanosecond, please consider the possibility we're wandering around the little pearly Schwartzsee snorkeling around the seaside, or going take our cubs at school, or any other human type of activity: just be patient, we'll come; and if you are really interested in getting the solution worked at, please document as throughly as possibly your discovery or necessity, and file an "issue" on the Github repository - easy task).


## History

The actual _pb_met_ has originally been developed by [Servizi Territorio srl](http://www.serviziterritorio.it) under the name PBL_MET by the pioneering effort of Roberto Sozzi and Daniele Fraternali, and used there internally until currently in their consultancy activities.

The first version of PBL_MET has also been presented at the [2nd Workshop on Harmonisation within Atmospheric Dispersion Modelling for Regulatory Purposes](http://www.harmo.org/Conferences/Proceedings/complist.asp#2nd), held in Manno (Switzerland) in 1993.

For a time, PBL_MET has been also sold as a software product by Servizi Territorio srl.

The original contributors were Daniele Fraternali and Roberto Sozzi. Their version is still retained, for completeness and historical fidelity. Besides, our gratitude to them and their work - and to the various nameless people who from time to time contribute to the "old" code.

For user convenience we have also placed the old PBL_MET in directory "PBL_MET_old" on GitHub source repository: that was our starting point, and to some extent it may still be useful. But please, consider its interest today is mainly historical. With time, more and more of it will be replaced by more up-to-date routines, placed in other repository directories.

Old PBL_MET users will discover the scope of the new _pbl_met_ has expanded somewhat. In addition to Planetary Boundary Layer related routines, now a directory dealing with "instrumentation" has appeared. This was an inevitable consequence of the technical evolution in the field, no less quick than in any other.


## And now...

Since then the library has been expanded, revised and tested to the extent possible. In 2015, a task has been undercome by Servizi Territorio srl to place the current version of PBL_MET, now named _pbl_met_, in the open-source universe, as a mean of sharing knowledge and, yes, as a capability demonstrator ([Servizi Territorio srl](http://www.serviziterritorio.it) wins part of her bread by running atmospheric dispersion models, an activity demanding massive meteorological data quality assurance and processing).



# Using _pbl_met_

## Installation

To install _pbl_met_ you may just copy all the sources in `core` directory to a location of your choice, then open a terminal session and give the command

```
make
```

within of it.

To check the installation was successful, you should verify the existence of `pbl_met.mod`and `pbl_met.a` in your local directory; the latter file is the static library containing the _pbl_met_ functions and symbols.

Once you have  `pbl_met.mod`and `pbl_met.a` available, you may copy them to directories where Fortran module includes and libraries reside on your system, or to the directory where you're developing your application sources.

Fortran compilers provide command line options allowing to specify non-standard locations for library files and module includes: we have _not_ made use of them, in sake of simplicity.

## Optimizing for your platform

The `makefile` provided with _pbl_met_ is minimal, and contains no optimizations, nor platform-specific enhancements.

This is intentional, as addressing all possible platforms would be at least quite difficult.

Nonetheless, you might desire to optimize the _pbl_met_ library for your platform (and compiler). In case, you may want to change the `makefile` by adding the approprite compiler options.

## Using library functions

To use functions and symbols in _pbl_met_ you should first of all make sure to `use`the _pbl_met_ module in your source, like this:

```
program YourProgram

	use pbl_met
	
	implicit none
	
	... your code ...
	
end program YourProgram
```

You may then compile your code for example using the command line as

```
gfortran -o yrprg YourProgram.f90 pbl_met.a
```

You may want to add command line options to specify where library and module files are found. Refer to the documentation of your compiler to know which they are.

# Library contents

## Overall library organization

The _pbl_met_ library is a set of functions and symbols (constants, data types, …), all callable by users once a ` use pbl_met`declaration has added to your source.

For users and developers convenience all these elements have been stored in separate "secondary" modules, whose public interfaces are then made accessible by the catch-all `pbl_met` main module.

The secondary modules are:

* `pbl_base` - Useful constants and symbols
* `pbl_stat` - General-purpose statistical routines for elementary statistics, auto/cross-covariance and correlation, and more
* `pbl_time` - Functions dealing with dates and times, and various interesting astronomical quantities like solar declination, sunset/sunrise, and more
* `pbl_thermo` - Functions dealing with thermodynamics, psychrometry, thermal comfort and related things
* `pbl_evtrn` - Functions dealing with plant evapotranspiration
* `pbl_wind` - Functions dealing with mean wind; also containing some statistical functions specific to wind (made necessary by the nature of horizontal wind as a complex number)
* `pbl_turb` - Functions dealing with turbulent fluctuations in measured data, along with elementary eddy covariance and related stuff
* `pbl_simil` - Functions dealing with surface layer (Monin-Obukhov) similarity, including estimation of turbulent indicators
* `pbl_depth` - Functions dealing with Planetary Boundary Layer depth under stable, neutral and convective conditions

In principle, library users with little time or need/motivation to explore how individual functions are made do not need to know about this partitioning into sub-libraries. All they need is knowing how to compile and link the library to their program, a reference, and a good amount of optimistic  faith in canned packages and computer hardware.

But we know by experience that sooner or later the case presents when understanding something specific becomes of paramount importance, and then the desire comes to look behind the hood. Maybe, even, to improve something, or get it more up-to-date. To those brave users, and developers by and large, we committed a library organization which is as ordered and as understandable as possible. We ourselves make a good use of it, and declare that in some cases a healthy amount of "information un-hiding" is welcome: especially among scientific library users, people willing to understand what are they doing is welcome - as well as deciding to remain forever an _end-user_, although possible and legitimate, is committing themselves to be a danger to their (aware or implicit) clients.

Because of this, in all function descriptions, be them just lists or reference material, we will consistently sort anything by the sub-library they can be found in.

## Library contents, by secondary modules

### `pbl_base`: Useful constants and symbols

This module contains various important constants, in part used by some of the functions in other secondary modules (and described in their reference sections).

One of these constants is of very special interest, and ubiquitous use in `pbl_met`: the symbol `NaN`, used to denote missing or invalid values.

The value of `NaN` is a special case of a _non-signalling not-a-number_. Not-a-number values are an invention of IEEE 754 floating-point arithmetics, to date supported by practically any commercial hardware, and originally used to denote things as the real `sqrt(-1.)` and other "invalid" function results. Indeed two kind of not-a-number values are defined, signalling and non-signalling. The former, once encountered during program execution, usually results in an exception: the offending program hangs, and a (possibly cryptic) error message is printed. The latter is way more interesting: it combines with oher floating point values, at hardware speed, yielding other non-signalling not-a-number values, propagating the invalid state until results are written somewhere.

Testing whether a floating point value is a not-a-number is performed in Fortran by using the very common language extension intrinsic `isnan(flt)`.  This function is sometimes not supported by some compilers, but in case you may provide a version of it by using the curious property of not-a-number values of being not equal to themselves:

```
function isnan(x) result(isThisNaN)

	! Routine arguments
	real, intent(in)	:: x
	logical				:: isThisNan
	
	! Locals
	! -none-
	
	! Check the value is different from itself
	isThisNaN = (x /= x)
	
end function isnan
```

To use a function like the above version of `isnan` you should presumably disable some optimizations of your compiler, maybe using some command-line flags: please consult your compiler's documentation.

Of course, if your compiler does support `isnan` directly, we recommend you stick to that version and do not reinvent the wheel - as we said most Fortran compilers have an `isnan` intrinsic to date.

### `pbl_stat`: Statistics



### `pbl_time`: Dates, times and astronomy



### `pbl_thermo`: Thermodynamics, psychrometry, thermal comfort and related things



### `pbl_evtrn`: Evapotranspiration





### `pbl_wind`: Wind, and its statistics





### `pbl_turb`: Turbulence indicators from measured data and elements of eddy covariance

To date this module is a placeholder, still to be filled.



### `pbl_simil`: Surface layer (Monin-Obukhov) similarity

To date this module is a placeholder, still to be filled.



### `pbl_depth`: PBL depth estimates and related quantities

To date this module is a placeholder, still to be filled.
