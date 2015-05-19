//#include <R.h>
//#include <Rinternals.h>
//#include <R_ext/Rdynload.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
#define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

#ifndef swap
#define swap(a,b) temp=(a);(a)=(b);(b)=temp;
#endif


// Declare function prototypes to be exported to DLL
void emaFwd(double values[], double times[], int *n, double values_new[], double *tau, double *lastValue);
void smaFwd(double values[], double times[], int *n, double values_new[], double *tau);
void nonSortedPos(double a[], int *na, double b[], int *nb, int pos[]);
void rank(double values[], double times[], int *n, double values_new[], double *tau);
void rollingQuantile_II(double values[], double times[], int *n, double values_new[], double *tau);
void sortedPos(double a[], int *na, double b[], int *nb, int pos[]);
void sortedUnion(double a[], int *na, double b[], int *nb, double res[], int *length);
void triagArea(int *n, double a[], double b[], double *c, double *start, double *end, double *areas);

// Declare internal helper function prototypes
void quickselect(double values[], int *n, int *k, double *out);
void rollingQuantile_I_helper(double values[], double times[], int *n, int *m, double values_new[], double *tau);


// For each element of sorted vector a, determine maximum index j in b such that b[j] <= a[i], (0 if not found)
void sortedPos(double a[], int *na, double b[], int *nb, int pos[])
{
  int i=0, j=0, k=0;

  // Skip values of 'a' that are smaller than b[0]. These will be replaced by NA in R-interface function
  while ((k < *na) && (a[k] < b[0]))
  {
    k++;
  }

  for(i = k; i < *na; i++)
  {
    while ((j < *nb-1) && (a[i] >= b[j+1]))
    {
      j++;
    }
    pos[i] = j+1;   // Shift index by one for R
  }
}


// For each element of (not necessarily sorted) vector a, determine maximum index j in sorted vector b such that:
// b[j] <= a[i], (0 if not found)
void nonSortedPos(double a[], int *na, double b[], int *nb, int pos[])
{
  int i=0, j=0;

  for(i = 0; i < *na; i++)
  {
    // Check if even any element large enough
    if (a[i] < b[0])
      continue;

    j = 0;
    while ((j < *nb-1) && (a[i] >= b[j+1]))
    {
      j++;
    }
    pos[i] = j+1;   // Shift index by one for R
  }
}


// For two sorted vectors, return sorted union (of unique elements)
void sortedUnion(double a[], int *na, double b[], int *nb, double res[], int *length)
{
  int i=0, j=0, k=0;
  double last_value=min(a[0], b[0]) - 1;
  double next_value;

  // Fill 'res' with elements in 'a' and 'b', until one of them hits the end
  while ((i < *na) & (j < *nb))
  {
    // Determine next value to be inserted into 'res'
    if (a[i] < b[j]) {
      next_value = a[i];
      i++;
    } else {
      next_value = b[j];
      j++;
    }

    // Check that next_value > last_value (and not just equal)
    if (next_value > last_value) {
      res[k++] = next_value;
      last_value = next_value;
    }
  }

  // Fill 'res' with remaining elements
  while (i < *na) {
    next_value = a[i];
    i++;
    if (next_value > last_value) {
      res[k++] = next_value;
      last_value = next_value;
    }
  }
  while (j < *nb) {
    next_value = b[j];
    j++;
    if (next_value > last_value) {
      res[k++] = next_value;
      last_value = next_value;
    }
  }

  // Save length of merged array
  length[0] = k;
}


// Forward looking EMA
void emaFwd(double values[], double times[], int *n, double values_new[], double *tau, double *lastValue)
{
  int i=1;
  double factor;

  // Trivial case of zero time series length
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  values_new[*n-1] = *lastValue;
  for(i = *n-2; i>=0; i--)
  {
    factor = exp(-(times[i+1]-times[i]) / tau[0]);
    values_new[i] = values_new[i+1] * factor + values[i] * (1-factor);
  }
}



// Forwards(!) looking moving average
// Remark: implementation incomplete?
void smaFwd(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // tau        ... tau of moving average window

  int i, end_pos=*n-1;
  double rolling_sum=0;

  // Trivial case of zero time series length
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  for (i = *n-1; i >= 0; i--) {
    // Shrink window on the right
    while (times[end_pos] > times[i] + *tau) {
    	rolling_sum = rolling_sum - values[end_pos];
      end_pos--;
    }
    rolling_sum = rolling_sum + values[i];


    // Copy data in rolling window to temporary array

    // Calculate mean of values in window
    values_new[i] = rolling_sum / (end_pos - i + 1);
  }
}


// Rolling rank (equal to rollingQuantile*rolling_obs)
void rank(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // tau        ... tau of moving average window

  int i, j, equal, larger, start_pos=0;
  double value;

  // Trivial case of zero time series length
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  values_new[0] = 1;
  for (i = 1; i < *n; i++) {
    // Shrink window on the left
    // Remark: first condition even needed?
    while ((start_pos < i) & (times[start_pos] < times[i] - *tau))
      start_pos++;

    // Determine how many values in time window are equal to or larger than last value/
    // Remark: can be made more efficient similar to MA_I calculation
    larger = 0;
    equal = 0;
    value = values[i];
    for (j = start_pos; j < i; j++) {
      if (values[j] > value)
        larger++;
      else if (values[j] == value)
        equal++;
    }

    // Calculate quantile of last value among all values in window
    if (i > start_pos)
      values_new[i] = (i - start_pos + 1) - (larger + equal / 2.0);
    else
      values_new[i] = 1;
  }
}


// Helper function for rolling quantile type=1, mode=2
void rollingQuantile_I_helper(double values[], double times[], int *n, int *m, double values_new[], double *tau)
{
  // values     ... matrix of time series values
  // times      ... array of observation times matching time series values
  // n          ... number of rows in 'values'
  // m          ... number of columns in 'values'
  // values_new ... matrix (of same dimension as 'values') used to store output
  // tau        ... tau of moving average window

  int i, j, k, l, equal, larger, count, pos, pos2, start_pos=0;
  double value, value2;

  // Trivial case of zero time series length
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  for (i = 0; i < *n; i++) {
    // Shrink window on the left
    // Remark: first condition even needed?
    while ((start_pos < i) & (times[start_pos] < times[i] - *tau))
      start_pos++;

    // For each column/time_series ...
    for (j = 0; j < *m; j++) {
      // Extract information about latest time series value
      pos = i + j * (*n);
      larger = 0;
      equal = 0;
      count = 0;
      value = values[pos];

       // Determine how many values in time window are equal or larger than latest time series value
      for (k = start_pos; k <= i; k++) {
        for (l = 0; l < *m; l++) {
          pos2 = k + l * (*n);
          value2 = values[pos2];
          if (abs(value2 + 666666) > 1e-6) {  // number denoting missing data
            count++;
            if (value2 > value)
              larger++;
            else if (value2 == value)
              equal++;
          }
         }
      }

      // Calculate quantile of latest value j-th time series among all values in window
      if (count > 0)
        values_new[pos] = 1 - (larger + equal / 2.0)  / count;
      else
        values_new[pos] = 0.5;
      //values_new[pos] = start_pos;
     }
  }
}


// Rolling quantile II (time-weighted average fraction of observations below current observation)
void rollingQuantile_II(double values[], double times[], int *n, double values_new[], double *tau)
{
  // values     ... array of time series values
  // times      ... array of observation times matching time series values
  // n          ... length of 'values'
  // values_new ... array (of same length as 'values') used to store output
  // tau        ... tau of moving average window

  int i, j, start_pos=0;
  double start, value, larger, by;

  // Trivial case of zero time series length
  if (*n == 0)
    return;
  assert(*n > 0);
  assert(*tau > 0);

  values_new[0] = 0.5;
  for (i = 1; i < *n; i++) {
    // Shrink window on the left
    // Remark: first condition even needed?
    start = times[i] - *tau;
    while ((start_pos < i) & (times[start_pos] < start))
      start_pos++;

    /* Determine time-weighted fraction of observations in time window that are
    equal or larger than last value */
    larger = 0;
    value = values[i];
    for (j = start_pos; j < i; j++) {
      // Determine weight of observation
      by = times[j+1] - max(times[j], start);

      // Add weights up
      if (values[j] > value)
        larger += by;
      else if (values[j] == value)
        larger += by / 2;
    }

    // Calculate quantile of last value among all values in window
    values_new[i] = 1 - larger / (times[i] - max(times[0], start));
  }
}



// Return k-th smallest value (counting starts at zero) of an array (the input array will be rearranged)
// Based on code in "Numerical recipies in C", but with more meaningful variable names.
// See "http://ndevilla.free.fr/median/median/index.html" for a performance analysis.
void quickselect(double values[], int *n, int *k, double *out)
{
  // values ... array of values
  // n      ... length of array
  // k      ... return k-th smallest element
  // out    ... variable into which to write output

  unsigned int i, j, left, right, mid, found;
  double pivot, temp;
  left = 0;
  right = *n - 1;
  found = 0;

  for (;found == 0;) {
    if (right <= left + 1) {  // Array down to 1-2 elements
      if ((right == left + 1) && (values[right] < values[left])) {
        swap(values[left], values[right]);
      }
      out[0] = values[*k];
      left = right;
      found = 1;
    } else {
      // Select pivot element
      mid = (left + right) / 2;   // integer devision
      swap(values[mid], values[left + 1])
      if (values[left] > values[right]) {
        swap(values[left], values[right])
      }
      if (values[left + 1] > values[right]) {
        swap(values[left + 1], values[right])
      }
      if (values[left] > values[left + 1]) {
        swap(values[left],values[left + 1])
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
