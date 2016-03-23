An Unevenly-spaced Time Series (uts) is a sequence of observation time and value pairs (t<sub>n</sub>, X<sub>n</sub>) with strictly increasing observation times. As opposed to equally spaced time series, the spacing of observation times may not be constant.

##### Introduction

As of late 2011, to the best of my knowledge, there is no `R` time series package that allows to write 100% of the application logic in terms of this definition. Either directly or indirectly, existing implementations fall back on equally spaced data for some of their functionality. For example, the window width of a rolling time series operator, such as a moving average, is usually specified in terms of the number of observations (e.g. 7 observations) instead of a temporal duration (e.g. 6 hours).

Even when a time series is equally spaced, it is often preferable to define operations using a temporal duration (e.g. a moving average over the past year) instead of number of observations (e.g. a moving average over the last 12 observations values). Should the frequency of the time series change, then in the former case the code would not require any changes, while it would the later case. Moreover, in this way an identical analysis can be carried out on multiple time series of different frequencies, without having to keep track of the individual observation frequencies.

I therefore decided to design a new time series package, partially based on my [research](http://www.eckner.com/research.html) on this topic.

##### Advantages

* 100% of the application logic can be written in terms of temporal durations (as opposed to number of observations).
* Individual time series can store arbitrary `R` objects (not just numbers).
* Add-on packages provide novel functionality:
  * utsData: example data sets
  * utsMultivariate: time series vectors and matrices
    * These objects can be manipulated like normal R vectors and matrices, but with one extra dimension representing time.
    * The individual time series are completely independent of each other. In particular, the observation times do not need to be synchronized.
  * utsOperators: moving averages and other rolling time series operators
  * utsTrendSeasonality: trend and seasonality estimation

##### Comparison with other packages for irregular time series

* [zoo](http://cran.r-project.org/web/packages/zoo/index.html):
  * According to the package vignette, independence of a particular index/time/date class is the most important design goal, while `uts` relies on the [POSIXct](https://stat.ethz.ch/R-manual/R-devel/library/base/html/DateTimeClasses.html) class available in base `R`.
  * Most of the application logic is in terms of number of observations. For example, for subperiod selection, lagging of observations, and rolling time series operators.
  * The support for multivariate time series is somewhat restrictive. Individual time series need to have identical observation times and, in particular, need to be of the same length. Moreover, individual time series can only store objects of the same type, because the data is stored in a matrix, although there a plans to eventually supported mixed types via data frames.
* [xts](http://cran.r-project.org/web/packages/xts/index.html): An extension of `zoo` that facilitates inter-class operability.
* [fts](http://cran.r-project.org/web/packages/fts/index.html): An extension of `zoo` that provides many (stock) trading indicators and interface to `tslib` (a time series library in C++).
* [its](http://cran.r-project.org/web/packages/its/index.html): An S4 implementation with a few basic methods. By the same author as `fts`.
* [timeSeries](http://cran.r-project.org/web/packages/timeSeries/index.html): Focused on computational finance.
* [tseries](http://cran.r-project.org/web/packages/tseries/index.html): Focused on computational finance (GARCH, ARCH models) and times series analysis (ARMA models) for *evenly*-spaced time series. Also contains the `irts` class for irregular time series, but with little functionality.


##### Where to start

* A sample analysis (coming later)
* The package vignette (coming later)
