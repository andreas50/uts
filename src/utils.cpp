#include <Rcpp.h>


/*
 * For two sorted numeric vectors 'a' and 'b', for each element a[i] determine the number of
 * elements in 'b' that are less than or equal (leq) to this value.
 *
 * Equivalently, because the vectors are sorted, for or each element a[i] determine the maximum
 * index j such that b[j-1] <= a[i].
*/
// [[Rcpp::export]]
Rcpp::IntegerVector C_num_leq_sorted(const Rcpp::NumericVector& a, const Rcpp::NumericVector& b, double tolerance)
{
  // Allocate memory for output
  int na = a.size();
  int nb = b.size();
  Rcpp::IntegerVector res(na);
  
  // Core of algorithm
  for (int i=0, j=0; i < na; i++) {
    while ((j < nb) && (b[j] <= a[i] + tolerance))
	    j++;
    res[i] = j;
  }
  return res;
}


/*
 * For two sorted numeric vectors 'a' and 'b', for each element a[i] determine the number of
 * elements in 'b' that are less than this value.
 *
 * Equivalently, because the vectors are sorted, for or each element a[i] determine the maximum
 * index j such that b[j-1] < a[i].
 */
// [[Rcpp::export]]
Rcpp::IntegerVector C_num_less_sorted(const Rcpp::NumericVector& a, const Rcpp::NumericVector& b, double tolerance)
{
  // Allocate memory for output
  int na = a.size();
  int nb = b.size();
  Rcpp::IntegerVector res(na);
  
  // Core of algorithm
  for (int i=0, j=0; i < na; i++) {
    while ((j < nb) && (b[j] < a[i] + tolerance))
      j++;
    res[i] = j;
  }
  return res;
}


/* 
 * For two sorted numeric vectors, return a vector containing the sorted union of (unique) elements.
 * 
 * Values less than 'tolerance' apart are considered identical and ommitted.
 */
// [[Rcpp::export]]
Rcpp::NumericVector C_sorted_union(const Rcpp::NumericVector& a, const Rcpp::NumericVector& b, double tolerance)
{
  // Allocate memory for intermediate results
  int na = a.size();
  int nb = b.size();
  Rcpp::NumericVector tmp(na + nb);
  
  // Initialize previously inserted value to a dummy value smaller than all elements in 'a' and 'b'
  double previous_value, next_value;
  if (na == 0)
    previous_value = b[0] - tolerance - 1;
  else if (nb == 0)
    previous_value = a[0] - tolerance - 1;
  else
    previous_value = (a[0] < b[0] ? a[0] : b[0]) - tolerance - 1;
  
  // Fill the intermediate results vector with unique elements from 'a' and 'b'
  int i=0, j=0, num_unique=0;
  while ((i < na) || (j < nb)) {
    // Determine the next candidate value to be saved
    if ((i < na) && ((j == nb) || (a[i] < b[j]))) {
      next_value = a[i];
      i++; 
    } else {
      next_value = b[j];
      j++;
    }
    
    // Only save values that differ from the previously inserted value by more than 'tolerance'
    if (next_value > previous_value + tolerance) {
      tmp[num_unique] = next_value;
      previous_value = next_value;
      num_unique++;
    }
  }
  return head(tmp, num_unique);
}
