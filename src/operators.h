// Description: Implementation for the time series operators described in:
//              Eckner (2011), "Algorithms for Unevenly-Spaced Time Series"
// Copyright: 2011 by Andreas Eckner
// License: Copying and distribution of this file, with or without modification,
//          are permitted in any medium without royalty provided the copyright
//          notice and this notice are preserved.


#ifndef _operators_h
#define _operators_h


// Remark: To allow interfaces to other programming languages such as R, all
//         variables are either pointers or arrays.


// Simple moving averages (SMAs)
void sma_eq(double values[], double times[], int *n, double values_new[], double *tau);
void sma_eq_stable(double values[], double times[], int *n, double values_new[],
                   double *tau, int *obs_refresh);
void sma_last(double values[], double times[], int *n, double values_new[], double *tau);
void sma_lin(double values[], double times[], int *n, double values_new[], double *tau);


// Exponential moving averages (EMAs)
void ema_eq(double values[], double times[], int *n, double values_new[], double *tau,
      double *initial_value);
void ema_last(double values[], double times[], int *n, double values_new[], double *tau,
       double *initial_value);
void ema_lin(double values[], double times[], int *n, double values_new[], double *tau,
        double *initial_value);


// Rolling moments
void rolling_moment_eq(double values[], double times[], int *n, double values_new[],
                       double *tau, double *m);
void rolling_moment(double values[], double times[], int *n, double values_new[],
                    double *tau, double *m);
void rolling_moment_lin(double values[], double times[], int *n, double values_new[],
                        double *tau, double *m);


// Other time series functions
void rolling_obs(double values[], double times[], int *n, double values_new[], double *tau);
void rolling_sum(double values[], double times[], int *n, double values_new[], double *tau);
void rolling_max(double values[], double times[], int *n, double values_new[], double *tau);
void rolling_min(double values[], double times[], int *n, double values_new[], double *tau);
void rolling_median(double values[], double times[], int *n,
                    double values_new[], double *tau);
void rolling_quantile_eq(double values[], double times[], int *n,
                         double values_new[], double *tau);

#endif
