# The _ST-Me_ code

The _ST-Me_ is a _meteorological processor_ and meteorological data quality evaluator.

## Who made and/or uses it

_ST-Me_ has been written by Servizi Territorio srl for internal use, as a tool mainly for conducing environmental impact assessment studies.

## Role of _pbl_met_ in the construction of _ST-Me_

The _pbl_met_ library has been used to implement all physics, and a large part (about 80%) of data processing. The actual production of met input for the dispersion models supported is built without using _pbl_met_.
