![pbl_met_logo](./pbl_met_Logo.png)

# A short description of _pbl_met_


## What is it?

_pbl_met_ is a library composed by various Fortran modules, test programs and accompanying data. The purpose of _pbl_met_ is to alleviate the chore of writing data processing systems in the field of planetary boundary layer (PBL) meteorology, by providing routines computing or estimating the quantities commonly required by atmospheric dispersion models and other computing codes.

## What is the PBL, and why the logo?

Tha Planetary Boundary Layer (or just PBL for friends) is the this layer of troposphere connecting the Earth surface with the upper free atmosphere.

The PBL, whose thickness may change around the day from some tens to some hundreds of meters, is where most of us (in the sense of the sub-aerial biosphere) spend their entire life. The PBL is where the Earth surface exchanges energy and chemicals with air, plants evapotranspirate to, seeds and spores propagate, birds and insects fly, anthropogenic pollutants disperse and dilute, and more.

Quite literally, it is our home.

Henceforth, processes and phenomena occurring there, although often of _local_ span, and sometimes quite microscopic size, play a central role in our life. To say of one, it is the intense mixing naturally occurring within the PBL which removes that extremely toxic by-product of photosynthesis, oxygen, from the leaves of plants (where it would quickly cause the reversion from photosynthesis to the deadly photorespiration would it not be removed) to the lungs and tracheas and other interfacing structures of us oxygen-respirating living beings: just because of this, without PBL processes, life as we know it would not occur in sub-aerial environments. There are million others key interconnections among comparts of the Earth system, and the PBL plays a role in many of them, like a sort of _airlike blood_ of Gaia. There are many other "bloods" the planet uses of course, yet the PBL is one.

Small scale (hundreds of meters at most, as we've seen), short time span (in the order of one hour and less): an interesting combination, demanding special observation techniques and instruments. Examples are three-dimensional ultrasonic anemometers (and thermometers), multi-channel radiometers, fast-sampling gas analysers, and more - less and less costly, and more and more diffused. Instruments mean data, and data demand data processing, when they are. When they are needed, but no apt instrument is there, then demand turns towards estimation.

Processing and estimating, in turn, make things like _pbl_met_ quite useful.

A software library, like the _pbl_met_ after all is, is often something quite arid, at best a bit boring. So it needs a great logo!

Ours is, we hope, evocative: the _pbl_met_ stays at the middle of ground (water) and sky, just as the PBL itself does. You may bet on this, it is a bit "interdisciplinary".


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

You may want to add command line options to specify where library and module files are found. In general, refer to the documentation of your compiler to know which they are. For `gfortran`you would add something like

```
gfortran -J <pbl_met.mod path> -o yrprg YourProgram.f90 <pbl_met.a>
```

where `<pbl_met.mod path>` is he pathname of a directory where you can find `pbl_met.mod`, and `<pbl_met.a>`is the full pathname of `pbl_met.mod`.

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

#### Constants

This module contains various important constants, in part used by some of the functions in other secondary modules (and described in their reference sections).

One of these constants is of very special interest, and ubiquitous use in `pbl_met`: the symbol `NaN`, used to denote missing or invalid values.

The value of `NaN` is a special case of a _non-signalling not-a-number_. Not-a-number values are an invention of IEEE 754 floating-point arithmetics, to date supported by practically any commercial hardware, and originally used to denote things as the real `sqrt(-1.)` and other "invalid" function results. Indeed two kind of not-a-number values are defined, signalling and non-signalling. The former, once encountered during program execution, usually results in an exception: the offending program hangs, and a (possibly cryptic) error message is printed. The latter is way more interesting: it combines with oher floating point values, at hardware speed, yielding other non-signalling not-a-number values, propagating the invalid state until results are written somewhere.

#### The ` IniFile` class

##### Introduction

One operation common in meteorological (and non-meteorological) data processing is reading initialization data from a configuration file.

To date various popular formats exist for initialization files. It is worth mentioning among them the `NAMELIST` format introduced in Fortran (and indeed very easy to use). But indeed many other specifications exist, like for example JSON, YAML, XML, or LUA used as a configuration language.

One of the earliest, and still very popular, is the INI format introduced decades ago in the MSDOS and Windows world. An INI file is composed by zero or more sections, each having a name, and containing zero or more "key=value" statements.

The specific variant used in _pbl_met_ adds a few extensions to the usual INI form:

* Any character in a line following (and including) a '#' character is ignored - that is, treated as a comment.
* A section may have an empty name. This is the empty section, treated as any other sections.
* Two sections may have the same name. In this case, they are treated as if a unique section.
* The same key may appear more than once in the same section. In this case, of the two only the last one is used.
* The section name, key and value may contain non-ASCII character. The actua treatment and rendering of these is compiler-dependent.

This is an example of (useless) INI File.

```
# Example INI file, for testing purposes only

[General]
StationName = Schöner_Frühling  # Some non-ASCII characters
Lat         = 10.11
Lon         = 12.13

[Senseful]
Num_Lines = 5
Line_001  = I claim line 2 contains no reference to line 3

[Vectors]
v1 = 3.14, 6.26, 7.654321
v2 = 1, 2, 3, 4, 5, 6, 7, 8, 9, 0

[Senseful]
Line_002  = Yes. But consider this: this line does contain one reference to line 1
Line_003  =
Line_004  = A toast to this Test!
Line_005  = Olim lacus coluerat...

[] # Empty-name section
here = Here I am, anyway
```

##### Why an INI instead of a `NAMELIST`?

Modern Fortran has standardized the NAMELIST format since a long time, and all existing recent compilers provide an excellent support to this configuration form. So, why trying to imagine using another?

My answer, coming fro experience, is the NAMEFILE form is extremely powerful, but also rigid (the two attributes co-exists not by chance). But in many cases, a configuration may need a fuzzy, more flexible definition. For example, it would be taxing and insensitive to ask someone to insert in a configuration file rows and rows of obvious data whose value is the default. This might make the configuration file easier to read to a computer, but "idiotic" and irritating to a human. With an INI file, you have the opportunity to insert only the interesting values, leaving the others to the defaulting mechanism of the INI API - or class, in this case.

##### The overall structure of IniFile class

The IniFile class consists of a data type, with private data contents, along with some member functions:

* `read(iLUN, sIniFileName)` - Constructs an IniFile instance from an INI file.
* `dump()` - Print IniFile contents, without any comment, to display (may be redirected to file).
* `getString(sSection, sKey, sValue, sDefault)` - Retrieve a string.
* `getReal4(sSection, sKey, rValue, rDefault)` - Retrieve a single precision floating-point value.
* `getReal8(sSection, sKey, rValue, rDefault)` - Retrieve a double precision floating-point value.
* `getInteger(sSection, sKey, iValue, iDefault)` - Retrieve an integer value.

All these members are function returning an integer completion code. A value of 0 indicates uccessful completion, while a non-zero value indicates an error condition.

In case of errors, if present the `sDefault` value is returned in `sValue`. If `sDefault`, optional, is not specified then an invalid value is returned. This is a NaN for floating point values, a -9999 for integer values, and the empty string "" for strings.

#### Other symbols

The `pbl_base` module defines two new operators, `.valid.` and `.invalid.`, testing respectively if a floating point value is valid (i.e. not NaN) and invalid (that is, a NaN).

The operators can be used on scalars, as in

```
if((.valid.P) .or. (.invalid.Q)) then ...
```

But it can be also be used with vectors and multi-dimensional arrays, the underlying functions being declared `pure` and `elemental`, a Fortran 95 syntax specifying a function has no side effects and may be applied to an array, acting in case element-wise.

Use of operators on vectors combine with existing modern Fortran syntax, allowing to use expressive statements like

```
if(all(.valid.rvData)) then ...
```

This is another advantage of operator form.

Using `.valid.` and `.invalid.` in your end-user code instead of checking a value is NaN or not, makes your intentions clearer to your code readers.

#### An alternative to `.valid.` and `.invalid.` 

Testing whether a floating point value is a not-a-number may also be performed in Fortran by using the very common language extension intrinsic `isnan(flt)`.  This function is sometimes not supported by some compilers, but in case you may provide a version of it by using the curious property of not-a-number values of being not equal to themselves:

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

As mentioned in the previous section, `.valid.` and `.invalid.` operators are also available in _pbl_met_ to perform an equivalent task. In general, it is preferable (although not mandatory) to use the operators instead of ` isnan`, as the resulting code is clearer to readers: the operators convey a semantic meaning ` isnan` has not. In library code, however, both forms are used.

### `pbl_stat`: Statistics

#### Data types and classes

##### The `TimeSeries` class

###### Introduction

The `TimeSeries` is an object-oriented Fortran type, supporting the basic treatment of time series whose data contents is logically a set of ordered couples
$$
\left( t_{i}, x_{i} \right)
$$
where the index $i$ range extends over all integers between 1 and $n>0$ , inclusive.

In the _pbl_met_ actual implementation, univariate time series are made by two floating point vectors: one, of double precision (64 bit) type, holding the time stamp, and another, of single precision (32 bit), containing the corresponding series value. The two vectors are of identical length, and the ordered couples mentioned above are formed by elements having the same index value.

Both time stamp and data vectors are _not_ accessible by users of the `TimeSeries` class: any possible use, from creation through data population to processing, is performed using appropriate member functions. This approach allows to maintain vector integrity better than it would be possible allowing class users to access vectors directly, at the price of a bit of study more.

###### Why a vector of time series? Why not just an initial time and a time step? 

On a first superficial look, using a double precision vector to hold time stamps may seem a huge overhead. But, we have our good reasons.

The library _pbl_met_ is designed to operate on real-world data sets. These collections have been created using a variety of hardware devices whose components may fail, or be reconfigured by users, or changed in many ways. So, assuming all time stamps are exactly equally spaces and without gaps would have exposed _pbl_met_ to the risk of not being able to deal with real data.

Using a time stamp vector allows time stamps to be irregularly spaced (as they could be, for example, for data coming from IoT appliances where the data sampling time is not always defined unambiguously), and even to be not monotonically increasing (as it may happen because of a failure in the data logger real time clock).

This permissive approach is complemented by the existence of quite a number of member functions whose task is to test the time stamps of a specific time series possess some desirable properties, like being equally-spaced, containing no gaps or invalid times, and more. _pbl_met_ users  may then choose which actual properties to check (and then ensure) prior to perform their application-specific processing.

###### What about _multivariate_ time series?

The `TimeSeries`data type is by its very nature _univariate_. Many real-world time series, however, constitute _multivariate_ time series - think for example to the many measurements taken simultaneously by a meteorological station equipped with many instruments.

Treating multivariate time series is possible in _pbl_met_, but a bit indirectly.

First of all, for two univariate time series to be treated as parts of a larger multivariate time series, their time stamp vectors must contain all valid value, and be component wise identical (up to 4 times the machine epsilon for double precision variables). The truth of this condition can be tested with a specific member function, `isSameTimes`. This granted, _and_ if the time stamp vector common to the two time series is well-spaces, it makes sense to compute cross-quantities (like cross-covariances, cross-correlations), with lags having a precise time meaning.

###### Member procedures


#### Low-level procedures

##### Off-range and invalid data management
###### Subroutine `RangeInvalidate`

Make data outside a specified range invalid, by replacing their value with NaN.

Interface

```
subroutine RangeInvalidate(rvX, rMin, rMax)
    real, dimension(:), intent(inout)   :: rvX      ! Vector of data to range-invalidate
    real, intent(in)                    :: rMin     ! Minimum allowed value
    real, intent(in)                    :: rMax     ! Maximum allowed value
end subroutine RangeInvalidate
```

The data vector `rvX`may be both fixed-dimension and allocatable. In the latter case, it is the responsibility of user to make sure the vector is allocated on entry to this routine.

It must also be `rMin < rMax`.

The result is straightforward: assuming `rvX = [1., 2., 3., 4., 5.]` and `rMin = 2.`, `rMax = 4.`, then on exit we would have `rvX = [NaN, 2., 3., 4., NaN]`as both 1. and 5. are off.range. The two extremes `rMin, rMax` are used in inclusive mode, that is, if $v$ is a generic floating point value, it is accepted if

`rMin`$\le v \le$ `rMax`

###### Subroutine `PairInvalidate`

###### Subroutine `RangeClip`

###### Subroutine `GetValidOnly`

##### Basic statistics
###### Function `Mean`

###### Function `StdDev`

###### Function `Cov`

###### Why not a `Min`and `Max`?

##### Autocovariance, autocorrelation and related
###### Function `AutoCov`

###### Function `AutoCorr`

###### Function `PartialAutoCorr`

###### Function `EulerianTime`

##### Cross-covariance, cross-correlation and related
###### Function `CrossCov`

###### Function `CrossCorr`

###### Function `Utilities`

###### Subroutine `RemoveLinearTrend`

### `pbl_time`: Dates, times and astronomy

#### Date and times, according to the _pbl_met_

In many programming languages (R and Python among them), dates and times are supported either natively, or by standard libraries.

This is not the case with Fortran, at least to date, which provides no support at all for date and time values.

In past times, some vendors in reality implemented date and time routines in Fortran (then still named FORTRAN), but these goodies did not find their way into the language standards, and eventually began their steady way to oblivion.

But, Fortran is a language ideally suited to numerical applications, and implementing dates and times within of it is not difficult.

The real problem is, which of the subtleties connected with treating dates and times to implement, a subject around which it is not always simple to attract consensus. These subtleties range from leap year definitions (at least three among engineers), to leap seconds, passing through time zones.

We had to take our choice, and this is: in _pbl_met_ time zones are _not_ considered, and leap seconds do _not_ exist. Leap years are considered the most accurate way, instead.

These design assumptions are considered consistent with the _pbl_mt_ purpose, to support meteorological and micro-meteorological data processing. Then, contrary to what could happen in climatological applications, we'll typically deal with _short_ (few years at most) time spans, maybe projecting many years, even centuries in the past, and maybe of large memory size - think to a fast-sampling data acquisition system, for example. In these cases, time changes as the ones due to leap second may be ignored in most cases.

Date and time values in _pbl_met_ are then encoded numerically, as either `integer(4)` or `real(8)` depending on circumstances.

You, the user, are free to interpret these numerical values as you want, in particular for example if to consider them referred to UTC or some local time zone, but this is by no means mandatory. We suggest to adopt a local time however, as astronomical routines will use time and position to estimate their quantities, and for this to happen safely and sensibly knowledge of which the reference system is.

#### Time stamp values

##### What a time stamp is, exactly?

What is a "time stamp" might seem obvious on a first instance.

And presumably, it really is.

Nonetheless, let's us state a definition: A _time stamp_ is a date/time value attached as a label to an event occurring at a precise time instant, or interval.

The date/time values contained in time series vectors are, in strict sense, time stamps: labels, attached to the values having the same index value.

The time stamp may be absolutely precise, in the sense that it designates the exact instant at which the event did occur, with reference to some well-defined clock; but more often than no, it deviates somewhat from the instant it should designate. This happens because of the finite resolution and accuracy of the real time clocks found in data acquisition devices, and is to a large extent inevitable. By the way, the resolution allowed by _pbl_met_ if finer of most practically used real time clocks'.

In case of time stamps used to label time interval, in addition to isues of resolution and accuracy we have another, much larger one: inherent ambiguity. A time interval has by its very nature a finite positive time span, but is constituted by a continuum (discrete quantum mesh?) of individual time instants, and the problem arises of which one to select to designate the whole interval. In this respect various schools exist, of which two very popular, and one somewhat less frequent.

The two popular interval time stamping camps are the earlier (the time stamp of an interval is the time stamp of its initial instant - this camp mostly collects scientists). And, the laters (the time stamp of an interval is the time stamp of its final instant - this other camp is populated mostly by data acquisition system manufacturers, and, yes, engineers). The third, less popular but still living, camp is of those who stay in the middle - quite literally: the time stamp of an interval is the time stamp corresponding to its mid-point.

To make things a bit more confusing, we should define in some exact way what a "time interval" really is. We may assume it is a finite connected subset of the real line corresponding to all times from minus to plus infinity - this is the way people imagine a "time interval". But then, does the interval contain its extremal points, or not? It might not contain the final, or the initial, point for example, so to avoid the danger of counting a value lying on the extremum of an interval to be counted twice, once for the left and once for the right intervals it belongs to.

In view of such bewildering variety, _pbl_met_ allows transforming one kind of time stamp to the other, by allowing _time shifts_ on time stamp vectors.

##### Short time stamps

A pbl_met short time span is an integer(4) value representing the number of seconds occurred since the "local Epoch", defined as 1970-01-01 00:00:00 referred to the selected time zone.

Because of limitations in `INTEGER(4)` values, the maximum second-shift from the Epoch is 2147483647, corresponding to a maximum time stamp
`
2038-01-19 03:14:07
`

This may not be the sufficient for applications demanding to label events farther in future. This inconvenient is addressed by changing from short to long time stamp encoding (see next section).


##### Long time stamps

A _pbl_met_ long time stamp is a real(8) ("double precision") value, whose integer part represents the number of seconds from the Epoch of current date-and-time, and whose fractional part is the sub-second part of the time stamp.

In theory, a real(8) time stamp would be able to represent dates far if future by very many years. For practical reasons (mainly due to the need to let year to stay within 4 digits in printing, the maximum allowed date and time value has been fixed to `9999-12-31 23:59:59.999`). Epoch values larger than this value are not considered valid to specify a _pbl_met_ date and time.

##### Time stamp vectors, and the `DateTime` class

In addition to single-number time stamps, ideal to encode time values in computer memory, time can be specified by assigning separately the year, month, and so on. This is known in _pbl_met_ as a _time vector_.

Clearly, time vectors are less compact than integer(4) or real(8) based time stamps. But, they have a big advantage which in some circumstances may have a value: they can be easily understood by human readers.

The disadvantage associated is, every time you have to use one of them you have to declare and use six numbers instead of a single one - and this may be cumbersome, in the long term.

To mitigate this effect, the elements of a time vector may be placed into a user-defined data type. This way, they are declared all-in-one, and used group-wise.

A better even method is, defining a class which, in addition to the elements of time vector, also contains member functions performing some common operations like conversion to and from single-number time values, or formatting as an ISO date-time. This is precisely the task of `DateTime` class.

### `pbl_thermo`: Thermodynamics, psychrometry, thermal comfort and related things



### `pbl_evtrn`: Evapotranspiration





### `pbl_wind`: Wind, and its statistics

#### Wind vector

##### Definition

In _pbl_met_ the word  _wind_ is used to designate the airflow vector at a given point and time - the latter being a given instant or averaging period.

In general, the wind vector has three components, $$\left[u,v,w\right]$$ aligned to the $$x,y,z$$ axes of the reference frame in use. However, on flat terrain without obstruction and with uniform roughness the vertical component $$w$$ is small compared to the other two, and in many practical applications it is then neglected: we then speak of _horizontal wind_, $$\left[u,v\right]$$.

##### Cartesian and polar form

The horizontal wind may be expressed as a two-dimensional Cartesian vector $$\left[u,v\right]$$, the way we made in the previous section; but it may also be expressed in polar form as $$\rho \exp(i \delta)$$ where $$\rho$$ and $$\delta$$ are wind _speed_ and _direction_ respectively.

The way the polar form is written indicates an interesting thing which may be useful from time to time in practical applications: the horizontal wind may be thought as a complex number.

In meteorology, as in any other branch of the Earth sciences, the wind direction angle $$\delta$$ is expressed in degrees, and counted from North increasing clockwise, instead of using the "mathematical" convention of counting from East increasing anticlockwise. Using the symbol $$\alpha$$ to designate the same direction as $$\delta$$, expressed according to the mathematical convention, we have the following relationship:
$$
\delta = 90° - \alpha
$$
This given, we have the following conversion formula for horizontal wind speed components:
$$
\left[u,v\right] = \left[\rho \sin(\delta), \rho \cos(\delta) \right]
$$
This formula may be inverted. To do so, it is better to use complex notation however: if $$U = u + i v$$ is wind in complex form, then we can say
$$
\rho = \left| U \right|
$$
and
$$
\delta = 90° - \arg \left(U\right)
$$
(valid if $$\rho > 0$$).

It can be noticed that the formula for wind direction $$\delta$$ contains an inherent ambiguity, as the argument of a non-zero complex number is defined (in degrees) up to $$k$$ times 360°, where $$k$$ is any integer. To overcome the problem, it is customarily assumed the argument function is restricted to its main branch, so that
$$
-180° < \delta \le +180°
$$
or
$$
0° \le \delta < 360°
$$
depending on the taste of users. _pbl_met_, incidentally, adopts the second standard range for $$\delta$$.

The $$\arg$$ function of complex arithmetics is implemented in Fortran through the `atan2()`intrinsic function, so that the counterpart of the formula for $$\delta$$ is something like

```
dir = 180*atan2(u,v)/pi
```

where `pi` is a constant or variable containing the floating-point approximation of $$\pi$$.

##### Flow and provenance conventions for wind direction

In mathematics, a 2D vector expressed in polar form yields no ambiguity on the meaning of the direction value $$\alpha$$: it represents the direction towards which the vector points.

This is not the same with wind direction in meteorology. Historically, the first systematic observations of wind direction were made by mechanical sensors, whose secondary transducer was a potentiometer yielding a resistance proportional to the azimuth corresponding to the direction _from which_ wind is blowing. To be fair, in meteorology some people use the provenance convention so far described; some other people use the flow convention however, and a way must exist to convert from on to the other.

If $$\delta_{p}$$ designates the provenance direction and $$\delta_{f}$$ the flow direction, then
$$
\delta_{p} = \delta_{f} \pm 180°
$$

The plus and minus sign applied to the half angle is consequence of the angle ambiguity in modular arithmetics, and one can use the one more in line with personal taste.

The previous relation may be inverted, to yield
$$
\delta_{f} = \delta_{p} \mp 180°
$$
and here we see that it is not the specific input and output conventions, but rather whether they are opposite, or same.

#### Wind related functions

##### Conversion of horizontal wind between Cartesian and polar form

The conversion between Cartesian and polar form of horizontal wind is performed through two functions, `CartesianToPolar2` and `PolarToCartesian2`.

The interface of these functions are

```
function CartesianToPolar2(cartesian, interpretation) result(polar)
	
    ! Routine arguments
    real, dimension(2), intent(in)  :: cartesian    ! Wind in cartesian form (u=cartesian(1), v=cartesian(2))
    real, dimension(2)              :: polar        ! Wind in polar form (vel=polar(1), dir=polar(2))
    integer, intent(in), optional   :: interpretation
    
end function CartesianToPolar2
```

and

```
function PolarToCartesian2(polar, interpretation) result(cartesian)
	
    ! Routine arguments
    real, dimension(2), intent(in)  :: polar      ! Wind in polar form (vel=polar(1), dir=polar(2))
    real, dimension(2)              :: cartesian  ! Wind in cartesian form (u=cartesian(1), v=cartesian(2))
    integer, intent(in), optional   :: interpretation

end function PolarToCartesian2
```

The arguments common to the functions have the following meaning:

* `cartesian`: 2D vector $$(u,v)$$.
* `polar`: 2D vector $$(\rho, \delta)$$.
* `interpretation`: optional integer constant, with values `WCONV_SAME` (default; designates when both forms share the same wind direction convention), `WCONV_PROVENANCE_TO_FLOW` when wind in provenance convention is converted to flow convention, and `WCONV_FLOW_TO_PROVENANCE` when wind in flow convention is converted to provenance convention.

The conversion functions operate as described mathematically in the sections above. The exception is, when $$[Math Processing Error]\rho \approx 0$$ and $$[Math Processing Error](u,v) \approx (0,0)$$. In this case the conversion function yield an invalid wind; to place this (mathematically-imposed) rule in due perspective, we should consider that null wind does not exist in Nature - air is constantly moving - and zero wind is an artifact due to instrumental and data logger nuisances as finite resolution and activation threshold.

##### Conversion of 3D wind between Cartesian and polar form

The conversion between Cartesian and polar form of 3D wind is performed through two functions, `CartesianToPolar3` and `PolarToCartesian3`.

The interface of these functions are

```
function CartesianToPolar3(cartesian, interpretation) result(polar)
	
    ! Routine arguments
    real, dimension(3), intent(in)  :: cartesian    ! Wind in cartesian form (u=cartesian(1), v=cartesian(2), w=cartesian(3))
    real, dimension(3)              :: polar        ! Wind in polar form (vel=polar(1), dir=polar(2), w=polar(3))
    integer, intent(in), optional   :: interpretation
    
end function CartesianToPolar3
```

and

```
function PolarToCartesian3(polar, interpretation) result(cartesian)
	
    ! Routine arguments
    real, dimension(3), intent(in)  :: polar      ! Wind in polar form (vel=polar(1), dir=polar(2), w=polar(3))
    real, dimension(3)              :: cartesian  ! Wind in cartesian form (u=cartesian(1), v=cartesian(2), w=cartesian(3))
    integer, intent(in), optional   :: interpretation

end function PolarToCartesian2
```

The arguments common to the functions have the following meaning:

- `cartesian`: 2D vector $$(u,v)$$.
- `polar`: 2D vector $$(\rho, \delta)$$.
- `interpretation`: optional integer constant, with values `WCONV_SAME` (default; designates when both forms share the same wind direction convention), `WCONV_PROVENANCE_TO_FLOW` when wind in provenance convention is converted to flow convention, and `WCONV_FLOW_TO_PROVENANCE` when wind in flow convention is converted to provenance convention.

The conversion functions operate as described mathematically in the sections above. The exception is, when $$[Math Processing Error]\rho \approx 0$$ and $$[Math Processing Error](u,v) \approx (0,0)$$. In this case the conversion function yield an invalid wind; to place this (mathematically-imposed) rule in due perspective, we should consider that null wind does not exist in Nature - air is constantly moving - and zero wind is an artifact due to instrumental and data logger nuisances as finite resolution and activation threshold.

##### Classifying horizontal wind by speed

By "classifying wind by speed" we mean assigning one or more wind speed readings to speed classes, based on a table containing upper class limits $$L = \left\lbrace l_{1},l_{2},\ldots,l_{n} \right\rbrace$$ with $$l_{i} < l_{i+1}$$ for $$1 \le i \le n-1$$. A first implied class is $$l_{0}=0$$, and class index $$i$$ is assigned to speed $$U$$ whenever $$l_{i-1} < U \le l_{i}$$.

This rule is easy to implement algorithmically as a table search, starting to compare $$U$$ with $$l_{1}$$, proceding sequentially for increasing limit index, and stopping as soon as $$U \le l_{i}$$. If no $$l_{i}$$ is found larger or equal to $$U$$, then class $$n+1$$ is conventionally assigned.

Of course, this search procedure is "inefficient" in abstract terms: if, as we assumed, the $$l_{i}$$ form an ordered sequence, then a binary search could be worth. In practice however, for a "small" $$n$$ (in the order of 32 or less), the index manipulation overhead makes the linear search more advantageous; in addition, the straight linear search depicted here is simpler to figure out, and a good model of searching.

An example of wind classification by speed is when constructing wind speed histogram, in which case the count of values having the same class index yields the frequency of the corresponding class. In this case, the number of class limits is typically a very small number (say 5 or 6 in most cases).

In _pbl_met_ the original classification is extended by allowing an explicit zero limit to be specified (useful when counting the number of wind speed which are exactly equal to zero); allowing one or more class limits to be invalid (that is, `NaN`), in which case their index is ignored; and last, assuming the vector of class limits to be not ordered increasingly, in which case the corresponding classes are once again not considered.

Also, the special case when the speed class vector $$L$$ has zero length, or all its components are `NaN`, is allowed, in which case the conventional "invalid class" -9999 is assigned.

The classification function exists in two forms, namely a _scalar_ and a _vector_ one. The former has interface

```
function ClassVel(vel, rvLimitVel) result(iClass)
    real, intent(in)                :: vel        ! A scalar vaue representing
                                                  ! wind speed (m/s)
    real, dimension(:), intent(in)  :: rvLimitVel ! The class limit vector (m/s)
    integer                         :: iClass     ! The speed class assigned
                                                  ! (-9999 if invalid or not found)
end function ClassVel
```

The latter has the following interface:

```
function ClassVel(vel, rvLimitVel) result(iClass)
    real, dimension(:), intent(in)  :: rvVel      ! A vector representing
                                                  ! wind speed values (m/s)
    real, dimension(:), intent(in)  :: rvLimitVel ! The class limit vector (m/s)
    integer, dimension(size(rvVel)) :: ivClass    ! The speed classes assigned
                                                  ! (-9999 if invalid or not found)
end function ClassVel
```

Setting `ivClass` size to a length at least equal (and possibly exactly so) to the length of `rvVel` it is the User responsibility.

If `vel` is `NaN`, then `iClass=-9999` (integer numbers don't allow their own `NaN` value, so the special value -9999 -or less!- is used to indicate an invalid value). In the same way, if an `rvVel` value is `NaN` then its corresponding class is -9999. That is, the class of an invalid speed is itself invalid.

Use of the vector or scalar form is left to the user taste: the two form are computationally equivalent. The vector form is slightly faster than a `DO` cycle incorporating many scalar calls, but the difference of execution time might well be small to non-existent is compiler optimizations are allowed.

##### Classifying horizontal wind by direction

Classifying horizontal wind by direction is much the same as classifying by wind speed, with some important differences however:

* Wind direction instead of wind speed is used as classification criterion
* Wind direction classes are fixed size, instead of chosen size as for wind speed class limits; actual wind direction limits depend on the number of classes in the 0°-360° range, and whether classes are centered to zero, or the first class starts exactly at 0°.

As with `ClassVel` for wind speed, the wind direction classification function `ClassDir`exists in two variants, scalar and vector.

The scalar variant has the following interface:

```
function ClassDir(dir, iNumClasses, iClassType) result(iClass)
	
    ! Routine arguments
    real, intent(in)                :: dir
    integer, intent(in)             :: iNumClasses
    integer, intent(in), optional   :: iClassType
    integer                         :: iClass

end function ClassDir
```

And this is the vector variant:

```
function ClassDir(dir, iNumClasses, iClassType) result(ivClass)
	
    ! Routine arguments
    real, dimension(:), intent(in)  :: dir
    integer, intent(in)             :: iNumClasses
    integer, intent(in), optional   :: iClassType
    integer, dimension(size(dir))   :: ivClass

end function ClassDir
```

The parameter type selects the variant actually employed. Here is their meaning:

* `dir` is a floating point scalar or vector representing wind direction in ° from North, increasing clockwise. Which convention is used (namely, provenance or flow) is indifferent, and the only requirement is the programmer knows which it is.
* `iNumClasses` is a positive integer representing the number of direction classes in which wind direction will be classified. Classes do not overlap, so that their width, in degrees, is equal to $$\delta = \frac{360°}{iNumClasses}$$.
* `iClassType` is an integer flag, indicating how the extrema of each class are defined. The choices are
  * `WDCLASS_ZERO_CENTERED`, in which the first class has its center angle at 0°, and extrema at $$\gamma_{-} = 0° - \frac{\delta}{2}$$ and $$\gamma_{+} = 0° + \frac{\delta}{2}$$. All other classes follow without gaps, spanning the whole angle.
  * `WDCLASS_ZERO_BASED`, in which the first class has its lower extremum $$\gamma_{-} = 0°$$, and upper extremum at $$\gamma_{+} = 0° + \delta$$. All other classes follow without gaps, spanning the whole angle.
* `iClass` (scalar variant) or `ivClass` (vector variant) is the result. As usual, the output of `ClassDir` when an invalid wind direction is passed is -9999.

The classification is made by defining a corrected direction $$d$$, whose value coincides with the direction passed as argument augmented by $$\frac{\delta}{2}$$ if `WDCLASS_ZERO_BASED` is used for `iClassType`, the direction itself otherwise.

The class number is then assigned using an algebraic formula:
$$
c = \left \lfloor \frac{d}{\delta} \right \rfloor + 1 
$$
The properties of the "floor" function dictate the exact behaviour of `ClassDir` at class extrema, so that a given direction belongs to class $$j$$ if
$$
\delta_{-}+(j-1) \delta \le d<\delta_{+}+(j-1) \delta
$$
Direction values smaller that 0° or larger than 360° are converted to range $$[0°,360)$$ modularly.

##### Computing mean wind speed and direction from polar form wind data

Averaging a set of wind speed and direction values demands a bit of caution when the "vector" horizontal velocity is desired: just averaging out separately the speed and direction is not the right way of doing, as this would yield the scalar speed and a direction value which is likely wrong if data across North are encountered.

The correct approach is to transform polar wind data to Cartesian form, average the two _components_, and back-transforming to polar. This is the task of the `VectorDirVel` function, with interface

```
function VectorDirVel(rvVel, rvDir) result(polar)

    ! Routine arguments
    real, dimension(:), intent(in)   :: rvVel
    real, dimension(:), intent(in)   :: rvDir
    real, dimension(2)               :: polar
		
end function VectorDirVel
```

As usual, the `polar` two-component vector contains wind speed in component 1, and direction in component 2.

It is assumed that wind direction conventions are always the same when averaging.

Eventual invalid (`NaN`) values are discarded from the average; if all values are invalid, then `NaN` is returned in both components of `polar`.

If the two vectors have different or zero length then `NaN` is returned in both components of `polar`.



##### Computing scalar wind speed from instant speeds

The calculation of scalar wind speed is simpler than what encountered for vector wind speed and direction: scalar speed is "just" the arithmetical mean of measured wind velocities.

To compute the scalar speed the following function is used:

```
function ScalarVel(rvVel) result(vel)

    ! Routine arguments
    real, dimension(:), intent(in)   :: rvVel
    real                             :: vel
    
end function ScalarVel
```

The `rvVel` must have at least one element (possibly `NaN`). In case the `rvVel` vector is zero length, or none of its elements is valid, a `NaN` is returned. In case `rvVel` contains some valid elements, the scalar speed is computed from the valid elements only, discarding  invalid elements if any. 

##### Computing unit direction from instant directions

In mainstream practice, and some of the preceding procedures, we already met the mean vector direction, defined by the formula
$$
\overline \delta = \arctan \frac{\sum_{i=1}^{n} U_{i} \sin \delta_{i}}{\sum_{i=1}^{n} U_{i} \cos \delta_{i}}
$$
in which we can see how the vector direction is a sort of weighted mean, in which instant vectors with higher speed $$U_{i}$$ "count more".

What, in this formula, for individual vectors with $$U_{i}$$ small? The answer is simple: they contribute much less to the vector direction, compared to the higher speed vectors. If, meanwhile, slow wind points in a visually different direction than faster wind, then the picture given by the vector direction is biased in favor of the direction of the faster winds.

In some applications this is not a problem - namely, when we're interested in the overall movement of an air parcel this "biased" speed is exactly what we want, as faster wind vectors contribute more to the overall shift than the slower. In other situations, knowing the contribution to direction from the slower wind is paramount - for example during the management of a toxic release under slow wind conditions.

In these situations, a different definition od wind direction, which is independent on speed, can be used:
$$
\overline \delta_{u} = \arctan \frac{\sum_{i=1}^{n} \sin \delta_{i}}{\sum_{i=1}^{n} \cos \delta_{i}}
$$
This is _unit (mean) direction_, and its value may differ from ordinary vector direction if the directions of different speed cohorts in wind data differ significantly. Conversely, a unit direction close to vector direction indicates that in the data set considered the directions from different wind speed cohorts are very similar.

May actually happen in Nature to find directions associated to different speed cohorts with significant differences among them? The answer is, yes, it may happen. One example is the very different direction one finds between nocturnal "fast" catabatic wind and "slow" diurnal anabatic wind in fair-weather breeze regime. On a smaller time scale, a difference may be found if the overall flow partitions in a slow en-masse movement superposed on a periodic, faster, fluctuation as happens during wind meandering.

So, unit direction has its importance.

The actual calculation is performed through the ` UnitDir`function, with the following interface:

```
function UnitDir(rvDir) result(dir)

    ! Routine arguments
    real, dimension(:), intent(in)  :: rvDir
    real                            :: dir
    
end function UnitDir
```

Invalid values are discarded from the overall unit direction. If all direction values are invalid, or the length of the direction vector is zero, then an invalid (NaN) value is returned. 

##### Computing numerical wind roses

In meteorology, a _wind rose_ is defined as the joint frequency function of wind direction and speed, classified according to a suitable method.

In _pbl_met_, the classification is made as in `ClassVel` and `ClassDir` functions, so, in particular, by equal-size direction classes and configurable size velocity classes.

The numerical wind rose is calculated using `WindRose`  function, having the following interface.

```
function WindRose(vel, dir, rvVel, iNumClasses, iClassType, rmWindRose) result(iRetCode)
	
    ! Routine arguments
    real, dimension(:), intent(in)                  :: vel			! Wind speed observations (m/s)
    real, dimension(:), intent(in)                  :: dir			! Wind direction observations (°)
    real, dimension(:), intent(in)                  :: rvVel		! Wind speed class limits as in ClassVel (m/s)
    integer, intent(in)                             :: iNumClasses	! Number of direction classes as in ClassDir
    integer, intent(in), optional                   :: iClassType	! Type of direction classes as in ClassDir (WDCLASS_ZERO_CENTERED (default), or WDCLASS_ZERO_BASED)
    real, dimension(:,:), allocatable, intent(out)  :: rmWindRose	! Joint frequency table of wind speed and direction, aka "wind rose" (in tabular form) 
    integer                                         :: iRetCode
		
end function WindRose
```

where

* `vel` is the vector of wind speeds (m/s)
* `dir` is the vector of wind directions (°)
* `rvVel` is the vector containing the speed class limits (m/s)
* `iNumClasses` is the desired number of wind direction classes
* `iClassType`, optional, is an integer flag specifying how classes are computed (WDCLASS_ZERO_CENTERED, default; or WDCLASS_ZERO_BASED)
* `rmWindRose` is an allocatable two-dimensional array containing the frequencies per class; the first index relates to speed class, the second to direction class
* `iRetCode` is a return code indicating whether the function terminated correctly (value is 0 in case) or not (any other value).

The two data vectors `vel`  and `dir`  should be the same size (otherwise an error is returned). If they are, values of same index are considered to refer to the same individual wind data.

Invalid values in `vel`  or `dir`  or both are discarded from the frequency counts.

The frequency in the returned numerical wind rose is normalized so that all its elements are non-negative, and their sum is 1 - unless no (valid) data are presented to the function, in which case all values are zero.

If the return code is non-zero, that is, if an error has occurred, the numerical wind rose array can be not allocated. Prior using it it is then necessary to chech the return value.


### `pbl_turb`: Turbulence indicators from measured data and elements of eddy covariance

To date this module is a placeholder, still to be filled.



### `pbl_simil`: Surface layer (Monin-Obukhov) similarity

To date this module is a placeholder, still to be filled.



### `pbl_depth`: PBL depth estimates and related quantities

To date this module is a placeholder, still to be filled.

# References

|                 |                                                              |
| :-------------- | ------------------------------------------------------------ |
| Brockwell, 2002 | P.J. Brockwell, R.A. Davis, _Introduction to Time Series and Forecasting_, Springer, 2002 |
|                 |                                                              |
|                 |                                                              |
| Deserno, web1   | M. Deserno, "How to generate exponentially correlated Gaussian random numbers", https://www.cmu.edu/biolphys/deserno/pdf/corr_gaussian_random.pdf |
|                 |                                                              |
| Geiger, 2003    | R. Geiger, R.H. Aron, P. Todhunter, _The Climate Near the Ground_, 6th edition, Rowman & Littlefield, 2003 |
| Hardy, 1998     | B. Hardy, *ITS-90 Formulations for Vapor Pressure, Frostpoint Temperature, Dewpoint Temperature, and Enhancement Factors in the Range -100 to +100 C*, report, avaiable at URL https://github.com/serv-terr/pbl_met/tree/master/references/hardy_1998.pdf |
| Hyndman, 1996          | R.J. Hyndman, Y. Fan, "Sample Quantiles in Statistical Packages", _The American Statistician_, 50, 4, pp. 361-365, Nov 1996      |
| Ihaka, web1     | G.R. Ihaka, "Statistics 726, Assignment 2, Solution", https://www.stat.auckland.ac.nz/~ihaka/726/sol02.pdf, section1 on an implementation of Durbin-Levinson algorithm, written in R and producing the same result as R's "acf" function with `type="partial"`. |
|                 |                                                              |
|                 |                                                              |
| Paumier, 1986   | J. Paumier, D. Stinson, T. Kelly, C. Bollinger, J.S. Irwin. MPDA-1: meteorological processor for diffusion analysis - user's guide, US-EPA, 1986 |
|                 |                                                              |
| Sozzi, 2002     | R. Sozzi, M. Valentini, T. Georgiadis, _Introduzione alla turbolenza atmosferica - Concetti, stime, misure_, Pitagora, 2002 |
| Stull, 1988     | R.B. Stull, _An Introduction to Boundary Layer Meteorology_, Kluwer Academic Publishers, 1988 |
| Stull, 2015 | R.B. Stull, _Practical Meteorology: An Algebra-based Survey of the Atmospheric Science_, The University of British Columbia, 2015, downloadable at the University of British Columbia site from URL https://www.eoas.ubc.ca/books/Practical_Meteorology/prmet/PracticalMet_WholeBook-v1_00b.pdf |
|                 |                                                              |
| Venables, 2002  | W.N. Venables, B.D. Ripley, _Modern Applied Statistics with S_, 2nd edition, Springer, 2002 |
|                 |                                                              |
|                 |                                                              |


