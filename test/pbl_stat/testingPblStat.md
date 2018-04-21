# Testing _pbl_stat_ module

## Purpose of tests

The specific _pbl_stat_ test suite has two main purposes:

* Checking the delivery of correct results starting from gapless data (i.e. in an ideal situation).
* Verifying the yield of correct results using data with (a massive amount of) gaps.

Performing the tests also has some interesting side effects:

* _pbl_stat_ sub-library is demonstrated to successfully link with a user-written application.

_pbl_stat_ module functions are tested separately, and results compared to reference results computed using an R script; R results have been elected as reference data, as R is highly reputable in the field of statistics, and the correctness of its computations is proven by long successful use. Reference data has been computed in double precision, according to the default setting of R, and stored in text form writing all the decimals available. Computations in _pbl_met_ are made in single precision, so some degree of difference is expected.
