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
 * Assume given two sorted vectors 'a' and 'b'. For each element a[i], determine the number of
 * elements in 'b' than are less than or equal (leq) to this value.
 *
 * Equivalently, because the arrays are sorted, for or each element a[i] determine the maximum
 * index j in 'b' such that b[j-1] <= a[i].
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
