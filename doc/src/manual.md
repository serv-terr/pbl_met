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

If $$\delta_p$$ designates the provenance direction and $$\delta_f$$ the flow direction, then
$$
\delta_p = \delta_f \pm 180°
$$

##### Wind related functions

###### Conversion of horizontal wind between Cartesian and polar form

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

###### Conversion of 3D wind between Cartesian and polar form

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
| Ihaka, web1     | G.R. Ihaka, "Statistics 726, Assignment 2, Solution", https://www.stat.auckland.ac.nz/~ihaka/726/sol02.pdf, section1 on an implementation of Durbin-Levinson algorithm, written in R and producing the same result as R's "acf" function with `type="partial"`. |
|                 |                                                              |
|                 |                                                              |
|                 |                                                              |
|                 |                                                              |
| Sozzi, 2002     | R. Sozzi, M. Valentini, T. Georgiadis, _Introduzione alla turbolenza atmosferica - Concetti, stime, misure_, Pitagora, 2002 |
| Stull, 1988     | R.B. Stull, _An Introduction to Boundary Layer Meteorology_, Kluwer Academic Publishers, 1988 |
|                 |                                                              |
|                 |                                                              |
| Venables, 2002  | W.N. Venables, B.D. Ripley, _Modern Applied Statistics with S_, 2nd edition, Springer, 2002 |
|                 |                                                              |
|                 |                                                              |


