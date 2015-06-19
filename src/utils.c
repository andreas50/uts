/*
 * Various helper functions
 */

//#include <Rinternals.h>
//#include <R_ext/Rdynload.h>
//#include <assert.h>
//#include <math.h>
//#include <stdlib.h>
//#include <stdio.h>


#ifndef min
#define min(a,b) (((a) < (b)) ? (a) : (b))
#endif


/*
 * Assume given two sorted vectors 'a' and 'b'. For each element a[i], determine the number of
 * elements in 'b' than are less than or equal (leq) to this value.
 *
 * Equivalently, because the arrays are sorted, for or each element a[i] determine the maximum
 * index j such that b[j-1] <= a[i].
*/
void num_leq_sorted_arrays(const double a[], const int *na, const double b[], const int *nb, int pos[])
{
  unsigned int i=0, j=0;

  for (i = 0; i < *na; i++) {
    while ((j < *nb) && (b[j] <= a[i]))
	  j++;
    pos[i] = j;
  }
}


/* 
 * For two sorted arrays, return an array containing the sorted union of (unique) elements
 * as well as the length of the output array.
 * 
 * Values less than 'tolerance' apart are considered identical and ommitted.
 */
void sorted_union(const double a[], const int *na, const double b[], const int *nb, const double *tolerance,
                  double res[], int *length)
{
  unsigned i=0, j=0, k=0;
  double previous_value, next_value;
  
  // Initialize previously inserted value to a value smaller than all elements in 'a' and 'b'
  if (*na == 0)
    previous_value = b[0] - 1;
  else if (*nb == 0)
    previous_value = a[0] - 1;
  else
    previous_value = min(a[0], b[0]) - 1;
  
  // Fill the output array with elements from 'a' and 'b'
  while ((i < *na) | (j < *nb)) 
  {
    // Determine the next candidate value to be saved
    if ((i < *na) && ((j == *nb) | (a[i] < b[j]))) {
      next_value = a[i];   
      i++; 
    } else {
      next_value = b[j];
      j++;
    }
    
    // Only save values larger than the previous inserted value + eps
    if (next_value > previous_value + *tolerance) {
      res[k] = next_value;
      previous_value = next_value;
      k++;
    }
  }  
 
  // Save the length of the merged array
  *length = k;
}
