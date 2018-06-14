#include <Rcpp.h>

extern "C" {
#include "utils.h"
}


// [[Rcpp::export]]
Rcpp::IntegerVector C_num_leq_sorted(Rcpp::NumericVector a, Rcpp::NumericVector b, const double tolerance)
{
  int na = a.size();
  int nb = b.size();
  Rcpp::IntegerVector res(na);
  
  num_leq_sorted(a.begin(), &na, b.begin(), &nb, const_cast<double*>(&tolerance), res.begin());
  return res;
}


// [[Rcpp::export]]
Rcpp::IntegerVector C_num_less_sorted(Rcpp::NumericVector a, Rcpp::NumericVector b, const double tolerance)
{
  int na = a.size();
  int nb = b.size();
  Rcpp::IntegerVector res(na);
  
  num_less_sorted(a.begin(), &na, b.begin(), &nb, const_cast<double*>(&tolerance), res.begin());
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector C_sorted_union(Rcpp::NumericVector a, Rcpp::NumericVector b, const double tolerance)
{
  int na = a.size();
  int nb = b.size();
  int output_length;
  Rcpp::NumericVector tmp(na + nb);
  
  // Determine unique elements as well as their number
  sorted_union(a.begin(), &na, b.begin(), &nb, const_cast<double*>(&tolerance), tmp.begin(), &output_length);
  
  // Copy unique elements over to results vector
  Rcpp::NumericVector res(output_length);
  for (int i=0; i < output_length; i++)
    res[i] = tmp[i];
  return res;
}
