# The _genrad_ procedure

## Generating reference global radiation values

Measuring global solar radiation is notoriously tricky, instrument idiosyncrasies often adding up to non-ideal conditions. And besides, extant pyranometers tend to change their translation curve with time, so that calibration is recommended on a systematic schedule.

In view of evaluation, and validation, of measured global radiation it would be really useful to have some reference values to which data may be compared.

This is one of the jobs for _genrad_, a point-wise global radiation simulator.

The kind of simulations _genrad_ is able to perform fall in two categories:

* A worst (widest) case analysis in which radiation is estimated under changing temperature, relative humidity and pressure conditions.
* A stricter case in which temperature, relative humidity and air pressure are known.

## Running _genrad_

