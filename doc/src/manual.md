# _pbl_met_, a meteorological data processing library.

The _pbl_met_ is a library of related functions aimed at making the life of people willing to construct meteorological processors and other met data processing software lighter, by collecting together various common-use methods and formulae, and providing access to them in a simple way.

## Why Fortran?

The _pbl_met_ library is written in modern Fortran. Choosing a programming language is never a simple task, and to some extent tends to have irreversible consequence. We could have opted for some other popular data processing language like Python, or R. But this would have been of limited usefulness to our reference community. Practically all meteorological models, wind fields, pollutant dispersion models, eddy covariance sytems, and other codes dealing with the atmosphere are written in Fortran. A Fortran library would have then been interesting, and immediately useable in the context of meteorology.

In addition to this (essential) consideration, modern Fortran has some distinct advantages.

* It is an efficient language, allowing to build high performance executable - a must, in a field where often the processing operates on a gargantuan mass of data, or immense computing grids.

* It is conceptually simple. Since the inception of the Fortran 95 standard the language contains syntactical provisions for dealing with arrays, matrices and vectors. Fortran 2003 introduced full featured object-orientation and some welcome standard modules. Fortran 2008 made a huge effort to introduce various form of explicit parallelism. Nevertheless, all these changes did not change the essential nature of the Fortran language as a syntactically easy to learn and remember. A Fortran program written thinking to people will be easy to understand. Even though very powerful and expressive, the Fortran language contains practically no constructs which, used the wrong way, may wreak havoc in the form of subtle bugs or idiosynchrasies.

Because of all that, modern Fortran is quite ideal as a language for coding a physically inclined library as the _pbl_met_ is, in a way which promotes code self-documentation.

## Coding style


