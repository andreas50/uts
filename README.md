An Unevenly-spaced Time Series (uts) is a sequence of observation time and value pairs (t<sub>n</sub>, X<sub>n</sub>) with strictly increasing observation times. As opposed to equally spaced time series, the spacing of observation times may not be constant.

As of late 2011, to the best of my knowledge, there is no R time series package that provides 100% of the application logic in terms of this definition. Either directly or indirectly, existing implementations fall back on equally-spaced data for some of their functionality. For example, the window width of a rolling time series operator might be specified in term of the number of observations (e.g. 7 observations) instead of a temporal duration (e.g. 6 hours).

I therefore decided to write my own implementation, based on my [research](http://www.eckner.com/research.html) on this topic.

### Comparison with other packages for irregular time series

* zoo: provides a hybrid approach between evenly- and unevenly-spaced time series. For example, the application logic of rolling time series operators is in terms of the number of observations.
* its:
* irts:

### Where to start

* A sample analysis (coming later)
* The package vignette (coming later)
