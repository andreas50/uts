#include "utils.h"


/*
 * For two sorted numeric vectors 'a' and 'b', ror each element a[i] determine the number
 * of elements in 'b' that are less than or equal (leq) to this value.
 *
 * Equivalently, because the vectors are sorted, for or each element a[i] determine the maximum
 * index j such that b[j-1] <= a[i].
*/
void num_leq_sorted(const double a[], const int *na, const double b[], const int *nb,
  const double *tolerance, int num_leq[])
{
  for (int i=0, j=0; i < *na; i++) {
    while ((j < *nb) && (b[j] <= a[i] + *tolerance))
	    j++;
    num_leq[i] = j;
  }
}


/*
 * For two sorted numeric vectors 'a' and 'b', for each element a[i] determine the number
 * of elements in 'b' that are less than this value.
 *
 * Equivalently, because the vectors are sorted, for or each element a[i] determine the maximum
 * index j such that b[j-1] < a[i].
 */
void num_less_sorted(const double a[], const int *na, const double b[], const int *nb,
  const double *tolerance, int num_less[])
{
  for (int i=0, j=0; i < *na; i++) {
    while ((j < *nb) && (b[j] < a[i] + *tolerance))
      j++;
    num_less[i] = j;
  }
}


/* 
 * For two sorted numeric vectors, return a vector containing the sorted union of (unique) elements
 * as well as the length of the output vector.
 * 
 * Values less than 'tolerance' apart are considered identical and ommitted.
 */
void sorted_union(const double a[], const int *na, const double b[], const int *nb,
  const double *tolerance, double res[], int *output_length)
{
  int i=0, j=0, k=0;
  double previous_value, next_value;
  
  // Initialize previously inserted value to a dummy value smaller than all elements in 'a' and 'b'
  if (*na == 0)
    previous_value = b[0] - *tolerance - 1;
  else if (*nb == 0)
    previous_value = a[0] - *tolerance - 1;
  else
    previous_value = (a[0] < b[0] ? a[0] : b[0]) - *tolerance - 1;
  
  // Fill the output vector with elements from 'a' and 'b'
  while ((i < *na) || (j < *nb)) {
    // Determine the next candidate value to be saved
    if ((i < *na) && ((j == *nb) || (a[i] < b[j]))) {
      next_value = a[i];   
      i++; 
    } else {
      next_value = b[j];
      j++;
    }
    
    // Only save values larger than (previous_inserted_value + tolerance)
    if (next_value > previous_value + *tolerance) {
      res[k] = next_value;
      previous_value = next_value;
      k++;
    }
  }  
 
  // Save the length of the merged vector
  *output_length = k;
}
