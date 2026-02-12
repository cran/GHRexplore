# GHRexplore 0.2.2

* Added functions `epiyw_to_date()` and `date_to_epiyw()` to convert between dates and 
epidemiological year-weeks for all days of the week.
* Removed ISOweek dependency. 
* Improved visualization of short time series.
* Minor refactoring and lint.

# GHRexplore 0.2.1

* Fixed bug in `plot_correlation()` where negative circles were not showing.
* Added ellipsis argument to `plot_bivariate()` to increase flexibility.
* Minor documentation updates.

# GHRexplore 0.2.0

* New plotting function: `plot_timeseries2()` for dual-axis time series plots.
* Increased flexibility of `plot_map()` (line colour, width, alpha).
* *by_year* argument deprecated in `plot_map()` and substituted by *aggregate_time*.

# GHRexplore 0.1.1

* Fixed tests failing with cowplot 1.2.0.
* Fixed minor bug in `plot_map()`.

# GHRexplore 0.1.0

* Initial CRAN submission.



