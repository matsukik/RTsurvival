# RTsurvival

[![Travis-CI Build Status](https://travis-ci.org/matsukik/RTsurvival.svg?branch=master)](https://travis-ci.org/matsukik/RTsurvival)

A set of functions to perform survival analysis on reaction time (RT) data. The packages are modification and R port of the MATLAB routines developed by [Eyal Reingold](https://www.utm.utoronto.ca/psychology/people/faculty/eyal-reingold) and [Heather Sheridan](http://www.albany.edu/psychology/60729.php) in [Reingold & Sheridan (2014)] (http://journal.frontiersin.org/article/10.3389/fpsyg.2014.01432/full). The original MATLAB routines is a part of Supplementary Material in Reingold & Sheridan (2014), and is licensed under [Creative Commons Attribution License (CC BY)](https://creativecommons.org/licenses/by/3.0/).


This package implements following routines:

 - `surv.mean` which computes the mean survival curves (or difference curve) of RT data from a two condition experiment with multiple participants.
 - `DPA.orig` which performs divergence point analysis that were originally introduced in Reingold, Reichle, Glaholt, & Sheridan (2012).
 - `DPA.ci` which estimates the divergence point and its confidence interval, so that the significance of the difference between experimental conditions can be assessed.
 - `DPA.id` which estimates the individual specific divergence point for each participants.

All of the above routine comes with associated `plot` function for easy plotting.

To install the RTsurvival package from GitHub, first install devtools and then run
```
devtools::install_github("matsukik/RTsurvival")
```

Any pull/feature requests, bug reports, etc. are welcome!


