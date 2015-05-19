// Description: Implementation for the time series operators described in:
//              Eckner (2011), "Algorithms for Unevenly-Spaced Time Series"
// Copyright: 2009, 2010, 2011 by Andreas Eckner
// License: Copying and distribution of this file, with or without modification,
//          are permitted in any medium without royalty provided the copyright
//          notice and this notice are preserved.

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "operators.h"

#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
#define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

#ifndef swap
#define swap(a,b) {temp=(a); (a)=(b); (b)=temp;}
#endif


/******************* Helper functions ********************/

// Return smallest element in an array of doubles
double array_min(double values[], int n)
{
  int i;
  double min_value = values[0];

  for (i = 1; i < n; i++)
    min_value = min(min_value, values[i]);
  return min_value;
}


// Return sum of elements in an array of doubles
double array_sum(double values[], int n)
{
  int i;
  double sum = 0;

  for (i = 0; i < n; i++)
    sum += values[i];
  return sum;
}


// Return average of elements in an array of doubles
// Remark: numerically stable version for very long arrays (but slower)
double array_avg(double values[], int n)
{
  int i;
  double avg = 0;

  for (i = 0; i < n; i++)
    avg += values[i] / n;
  return avg;
}


// Calculate k-th largest (counting starts at zero) value of an array
// -) "quickselect" algorithm
// -) the input array will be rearranged
// -) runs in O(n) time
void kth_largest(double *values, int *n, int *k, double *out)
{
  // values ... array of values
  // n      ... length of array
  // k      ... return k-th smallest element
  // out    ... variable into which to write output

  int i, j, left, right, mid, found;
  double pivot, temp;
  left = 0;
  right = *n - 1;
  found = 0;

  while (found == 0) {
    if (right <= left + 1) {  // Array down to 1-2 elements
      if ((right == left + 1) && (values[right] < values[left])) {
        swap(values[left], values[right])
      }
      out[0] = values[*k];
      left = right;
      found = 1;
    } else {
      // Select pivot element
      mid = (left + right) / 2;   // integer devision
      swap(values[mid], values[left + 1]);
      if (values[left] > values[right]) {
        swap(values[left], values[right])
      }
      if (values[left + 1] > values[right]) {
        swap(values[left + 1], values[right])
      }
      if (values[left] > values[left + 1]) {
        swap(values[left], values[left + 1])
      }

      // Put smaller elements to left of pivot, larger to right
      i = left + 1;
      j = right;
      pivot = values[left + 1];
      for (;;) {
        do i++; while (values[i] < pivot);
        do j--; while (values[j] > pivot);
        if (j < i) break;
        swap(values[i], values[j])
      }
      values[left + 1] = values[j];
      values[j] = pivot;
      if (j >= *k)
        right = j-1;
      if (j <= *k)
        left = i;
    }
  }
}


/****************** END: Helper functions ****************/


// SMA_eq(X, tau)
void sma_eq(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... width of rolling window

  int i, left = 0;
  double roll_sum = 0;

  // Error checking
  assert(*n >= 0);
  assert(*tau > 0);

  for (i = 0; i < *n; i++) {
    // Expand window on right
    roll_sum = roll_sum + values[i];

    // Shrink window on the left to get half-open interval
    while (times[left] <= times[i] - *tau) {
      roll_sum = roll_sum - values[left];
      left++;
    }

    // Calculate mean of values in rolling window
    values_new[i] = roll_sum / (i - left + 1);
  }
}


// SMA_eq with 'roll_sum' occasionally calculated from scratch for numerical staility
// Remark: probably not needed unless both *n and * tau are very very large
void sma_eq_stable(double values[], double times[], int *n, double values_new[],
                   double *tau, int *obs_refresh)
{
  // values       ... array of time series values
  // times        ... array of observation times
  // n            ... number of observations, i.e. length of 'values' and 'times'
  // values_new   ... array of length *n to store output time series values
  // tau          ... width of rolling window
  // obs_refresh  ... after how many observations SMA should be recalculated from scratch

  int i, left = 0;
  double roll_sum = 0;

  // Error checking
  assert(*n >= 0);
  assert(*obs_refresh > 0);
  assert(*tau > 0);

  for (i = 0; i < *n; i++) {
    // Expand window on right
    roll_sum = roll_sum + values[i];

    // Shrink window on the left to get half-open interval
    while (times[left] <= times[i] - *tau) {
      roll_sum = roll_sum - values[left];
      left++;
    }

    // Recalculate 'roll_sum' from scratch if necessary
    if (i % *obs_refresh == 0) {
      values_new[i] = array_avg(&values[left], i - left + 1);
      roll_sum = values_new[i] * (i - left + 1);
      continue;
    }

    // Calculate mean of values in rolling window
    values_new[i] = roll_sum / (i - left + 1);
  }
}


// SMA_last(X, tau)
void sma_last(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... width of rolling window

  int i, left = 0;
  double t_left_new, roll_area, left_area;

   // Error checking
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  // Initialize output
  values_new[0] = values[0];
  roll_area = left_area = values[0] * (*tau);

  // Apply rolling window
  for (i = 1; i < *n; i++) {
    // Expand interval on right end
    roll_area += values[i-1] * (times[i] - times[i-1]);

    // Remove truncated area on left end
    roll_area = roll_area - left_area;

    // Shrink interval on left end
    t_left_new = times[i] - *tau;
    while (times[left] < t_left_new) {
      roll_area = roll_area - values[left] * (times[left+1] - times[left]);
      left++;
    }

    // Add truncated area on left end
    left_area = values[max(0, left-1)] * (times[left] - t_left_new);
    roll_area += left_area;

    // Save SMA value for current time window
    values_new[i] = roll_area / *tau;
  }
}


// SMA_lin(X, tau)
void sma_lin(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... width of rolling window

  int i, left = 0;
  double t_left_new, roll_area, left_area, width, weight, y2;

  // Error checking
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  // Initialize output
  values_new[0] = values[0];
  roll_area = left_area = values[0] * (*tau);

  // Apply rolling window
  for (i = 1; i < *n; i++) {
    // Expand interval on right end
    roll_area += (values[i] + values[i-1]) / 2 * (times[i] - times[i-1]);

    // Remove truncated area on left end
    roll_area = roll_area - left_area;

    // Shrink interval on left end
    t_left_new = times[i] - *tau;
    while (times[left] < t_left_new) {
      roll_area = roll_area - (values[left] + values[left+1]) / 2 *
                              (times[left+1] - times[left]);
      left++;
    }

    // Add truncated area on left end
    // Inline trapezoid() functionality to avoid function call overhead
    width = times[left] - t_left_new;
    if ((left == 0) | (width == 0))
      left_area = width * values[0];
    else {
      weight = width / (times[left] - times[left-1]);
      y2 = values[left-1] * weight + values[left] * (1 - weight);
      left_area = width * (y2 + values[left]) / 2;
    }
    roll_area = roll_area + left_area;

    // Save SMA value for current time window
    values_new[i] = roll_area / *tau;
  }
}


// EMA_eq(X, tau)
void ema_eq(double values[], double times[], int *n, double values_new[], double *tau,
            double *initial_value)
{
  int i;
  double w;

  // Error checking
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  // Calculate ema recursively
  values_new[0] = initial_value[0];
  for (i = 1; i < *n; i++) {
    w = exp(-(times[i]-times[i-1]) / *tau);
    values_new[i] = values_new[i-1] * w + values[i] * (1-w);
  }
}


// EMA_last(X, tau)
void ema_last(double values[], double times[], int *n, double values_new[], double *tau,
        double *initial_value)
{
  int i;
  double w;

  // Error checking
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  // Calculate ema recursively
  values_new[0] = initial_value[0];
  for (i = 1; i < *n; i++) {
    w = exp(-(times[i]-times[i-1]) / *tau);
    values_new[i] = values_new[i-1] * w + values[i-1] * (1-w);
  }

}


// EMA_lin(X, tau)
void ema_lin(double values[], double times[], int *n, double values_new[], double *tau,
        double *initial_value)
{
  int i;
  double w, w2, tmp;

  // Error checking
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  // Calculate ema recursively
  values_new[0] = initial_value[0];
  for (i = 1; i < *n; i++) {
    tmp = (times[i] - times[i-1]) / *tau;
    w = exp(-tmp);
    if (tmp > 1e-6)
      w2 = (1 - w) / tmp;
    else {
      // Use Taylor expansion for numerical stabiliy
      w2 = 1 - tmp/2 + tmp*tmp/6 - tmp*tmp*tmp/24;
    }
    values_new[i] = values_new[i-1] * w + values[i] * (1 - w2) + values[i-1] * (w2 - w);
  }
}


// rolling_moment_eq(X, tau, m) ... wrapper around SMA_eq
void rolling_moment_eq(double values[], double times[], int *n, double values_new[],
            double *tau, double *m)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... width of rolling window
  // m          ... which moment to calculate (non-negative number)

  int i;
  double *values2;

   // Error checking
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);
  assert(*m >= 0);

   // Allocate memory
  values2 = malloc(*n * sizeof(double));
  assert(values2 != NULL);

  // Calculate m-th power of observation values and apply SMA_eq
  for (i = 0; i < *n; i++)
    values2[i] = pow(values[i], *m);
  sma_eq(values2, times, n, values_new, tau);

  // Free memory
  free(values2);
}


// rolling_moment(X, tau, m) ... wrapper around SMA
void rolling_moment(double values[], double times[], int *n, double values_new[],
            double *tau, double *m)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... width of rolling window
  // m          ... which moment to calculate (non-negative number)

  int i;
  double *values2;

  // Error checking
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);
  assert(*m >= 0);

   // Allocate memory
  values2 = malloc(*n * sizeof(double));
  assert(values2 != NULL);

  // Calculate m-th power of observation values and apply SMA
  for (i = 0; i < *n; i++)
    values2[i] = pow(values[i], *m);
  sma_last(values2, times, n, values_new, tau);

  // Free memory
  free(values2);
}


// rolling_moment_lin(X, tau, m) ... wrapper around SMA_lin
// Note: calculates SMA_lin of X^p instead of linearly interpolating X and
//       then integrating the area under the curve.
void rolling_moment_lin(double values[], double times[], int *n, double values_new[],
            double *tau, double *m)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... width of rolling window
  // m          ... which moment to calculate (non-negative number)

  int i;
  double *values2;

   // Error checking
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);
  assert(*m >= 0);

   // Allocate memory
  values2 = malloc(*n * sizeof(double));
  assert(values2 != NULL);

  // Calculate m-th power of observation values and apply SMA_lin
  for (i = 0; i < *n; i++)
    values2[i] = pow(values[i], *m);
  sma_lin(values2, times, n, values_new, tau);

  // Free memory
  free(values2);
}


// Calculate rolling sum
void rolling_sum(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... width of rolling window

  int i, left = 0;
  double roll_sum=0;

  // Error checking
  assert(*n >= 0);
  assert(*tau > 0);

  for (i = 0; i < *n; i++) {
    // Expand window on the right
    roll_sum = roll_sum + values[i];

    // Shrink window on the left
    while (times[left] <= times[i] - *tau) {
      roll_sum = roll_sum - values[left];
      left++;
    }

    // Update rolling sum
    values_new[i] = roll_sum;
  }
}


// Calculate rolling number of observations
void rolling_obs(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times
  // n          ... number of observations, i.e. length of 'values' and 'times'
  // values_new ... array of length *n to store output time series values
  // tau        ... width of rolling window

  int i, left = 0;

  // Error checking
  assert(*n >= 0);
  assert(*tau > 0);

  for (i = 0; i < *n; i++) {
    // Shrink window on the left
    while (times[left] <= times[i] - *tau)
      left++;

    // Number of observations is equal to length of window
    values_new[i] = i - left + 1;
  }
}


// Calculate rolling maximum
void rolling_max(double *values, double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // tau        ... width of rolling window

  int i, j, left = 0, max_pos=0;

  for (i = 0; i < *n; i++) {
    // Expand window on right
    if (values[i] >= values[max_pos])
      max_pos = i;

    // Shrink window on the left to get half-open interval
    while (times[left] <= times[i] - *tau)
      left++;

    // Recalculate position of maximum if old maximum dropped out
    // Inline functionality of max_index() to avoid function call overhead
    if (max_pos < left) {
      max_pos = left;
      for (j = left+1; j <= i; j++)
        if (values[j] >= values[max_pos])
          max_pos = j;
    }

    // Save maxium for current time window
    values_new[i] = values[max_pos];
  }
}


// Calculate rolling minimum
void rolling_min(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // tau        ... width of rolling window

  int i, j, left = 0, min_pos=0;

  for (i = 0; i < *n; i++) {
    // Expand window on right
    if (values[i] <= values[min_pos])
      min_pos = i;

    // Shrink window on the left to get half-open interval
    while (times[left] <= times[i] - *tau)
      left++;

    // Recalculate position of minimum if old minimum dropped out
    // Inline functionality of min_index() to avoid function call overhead
    if (min_pos < left) {
      min_pos = left;
      for (j = left+1; j <= i; j++)
        if (values[j] <= values[min_pos])
          min_pos = j;
    }

    // Save minium for current time window
    values_new[i] = values[min_pos];
  }
}


// Calculate rolling median
void rolling_median(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // width      ... width of the window for calculating rolling maximum

  int i, j, window_length, k_low, k_high, left = 0;
  double *values2, val_low, val_high;

  // Allocate memory for temporary array (since median calculation scrambles data)
  values2 = malloc((*n + 1) * sizeof(double));

  for (i = 0; i < *n; i++) {
    // Shrink window on the left end
    while (times[left] <= times[i] - *tau)
      left++;

    // Copy data in rolling window to temporary array
    window_length = i - left + 1;
    for (j = 0; j < window_length; j++)
      values2[j] = values[left + j];

    // Calculate median of values in window
    // -) k_low and k_high calculation different from pseudocode because C arrays
    //    start at zero
    k_low = (window_length - 1) / 2;
    k_high = window_length - k_low - 1;
    kth_largest(values2, &window_length, &k_low, &val_low);

    if (k_low < k_high) {   // Even number of observations in rolling window
      // Get smallest element right of k_low-th largest
      val_high = array_min(values2 + k_high, window_length - k_high);
      values_new[i] = (val_low + val_high)/2;
    } else
      values_new[i] = val_low;
  }
  free(values2);
}


// Rolling quantile (equal weighting of observations)
void rolling_quantile_eq(double values[], double times[], int *n,
                         double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // tau        ... width of moving average window

  int i, j, equal, larger, left = 0;

  values_new[0] = 0.5;
  for (i = 1; i < *n; i++) {
    // Shrink window on the left end
    while ((left < i) & (times[left] <= times[i] - *tau))
      left++;

    // Determine how many values in time window are equal or larger than last value
    larger = 0;
    equal = 0;
    for (j = left; j < i; j++) {
      if (values[j] > values[i])
        larger++;
      else if (values[j] == values[i])
        equal++;
    }

    // Calculate quantile of last value among all values in window
    if (i > left)
      values_new[i] = 1 - (larger + equal / 2.0)  / (i - left);
    else
      values_new[i] = 1;
  }
}
