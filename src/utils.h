#ifndef _utils_h
#define _utils_h

void num_leq_sorted(const double a[], const int *na, const double b[], const int *nb, int num_leq[]);
void num_less_sorted(const double a[], const int *na, const double b[], const int *nb, int num_less[]);
void sorted_union(const double a[], const int *na, const double b[], const int *nb, const double *tolerance,
  double res[], int *length);

#endif