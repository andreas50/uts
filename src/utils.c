/*
 * Various helper functions
 */

//#include <Rinternals.h>
//#include <R_ext/Rdynload.h>
//#include <assert.h>
//#include <math.h>
//#include <stdlib.h>
//#include <stdio.h>


// Declare function prototypes to be exported to shared library
//void sorted_array_position(double a[], int *na, double b[], int *nb, int pos[]);


// Declare internal helper function prototypes
//void quickselect(double values[], int *n, int *k, double *out);
//void rollingQuantile_I_helper(double values[], double times[], int *n, int *m, double values_new[], double *tau);


/*
 * Assume given two sorted vectors 'a' and 'b'. For each element a[i], determine the maximum index j in
 * 'b' such that b[j] <= a[i], (-1 if not found).
*/
void sorted_array_position(const double a[], const int *na, const double b[], const int *nb, int pos[])
{
  unsigned int i=0, j=0, k=0;

  // Set index to -1 for values of 'a' that are smaller than all values in 'b'
  while ((k < *na) && (a[k] < b[0])) {
    pos[k] = -1;
    k++;
  }

  for(i = k; i < *na; i++) {
	while ((j < *nb - 1) && (a[i] >= b[j+1]))
      j++;
    pos[i] = j;
  }
}
