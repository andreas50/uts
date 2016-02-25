An Unevenly-spaced Time Series (uts) is a sequence of observation time and value pairs (t<sub>n</sub>, X<sub>n</sub>) with strictly increasing observation times. As opposed to equally spaced time series, the spacing of observation times may not be constant.

As of late 2011, to the best of my knowledge, there is no `R` time series package that allows to write 100% of the application logic in terms of this definition. Either directly or indirectly, existing implementations fall back on equally spaced data for some of their functionality. For example, the window width of a rolling time series operator, such as a moving average, is usually specified in terms of the number of observations (e.g. 7 observations) instead of a temporal duration (e.g. 6 hours).

Even when a time series is equally spaced, it is often conceptually preferable to define operations using a temporal duration, such as a "moving average over the past year", instead of using a number of observations. Should the frequency of the time series change at a later date, the code for the analysis would not need updating. Similarly, this way the same analysis can be carried out on several time series with different frequencies, without having to keep track of the individual observation frequencies.

I therefore decided to design a new time series package, partially based on my [research](http://www.eckner.com/research.html) on this topic.

##### Advantages

* 100% of the application logic can be in terms of temporal durations (as opposed to number of observations).
* Individual time series can store arbitrary R objects (not just numbers).
* Add-on packages provide novel functionality:
  * utsData: example data sets
  * utsMultivariate: time series vectors, matrices, and data frames
    * These objects can be manipulated like normal R vectors, matrices, and data frames, but with one extra dimension representing time.
    * The individual time series are completely independent of each. In particular, the observation times do not need to be
  * utsOperators: moving averages and other rolling time series operators
  * utsTrendSeasonality: trend and seasonality estimation

##### Comparison with other packages for irregular time series

* [fts](http://cran.r-project.org/web/packages/fts/index.html): A package with just a few basic functions.
* [its](http://cran.r-project.org/web/packages/its/index.html): An S4 implementation with a few basic methods. Not under active developement.
* [timeSeries](http://cran.r-project.org/web/packages/timeSeries/index.html): Focused on computational finance.
* [tseries](http://cran.r-project.org/web/packages/tseries/index.html): This package contains the `irts` class.
* [zoo](http://cran.r-project.org/web/packages/zoo/index.html):
  * According to the package vignette, independence of a particular index/time/date class is the most important design goal, while `uts` relies on the [POSIXct](https://stat.ethz.ch/R-manual/R-devel/library/base/html/DateTimeClasses.html) class available in base `R`.
  * Provides a hybrid approach between evenly- and unevenly-spaced time series. For example, the application logic of rolling time series operators is in terms of the number of observations.
  * [Take out after further review] Indexing into a time series is in terms of observation numbers (e.g. get observations at integer indices 5, 6, and 9), as opposed to in terms of observation times (e.g. get sampled observations at given time points).
  * The support for multivariate time series is somewhat restrictive. Individual time series need to have identical observation times and, in particular, need to be of the same length. Moreover, indicidual time series can only store objects of the same type, because the data is stored in a matrix, although there a plans to eventually supported mixed types via data frames.
* [xts](http://cran.r-project.org/web/packages/xts/index.html): An extension of `zoo` that facilitates inter-class operability.


##### Where to start

* A sample analysis (coming later)
* The package vignette (coming later)
