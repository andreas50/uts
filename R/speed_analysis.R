#############################################
# Speed analysis of R vs. C implementations #
#############################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3

# Conclusion: for long input vectors, the C implementation is around 14 times faster
if (0) {
  set.seed(1)
  a <- sort(runif(1e4))
  b <- sort(runif(1e4))
  
  # R vs. C: 3.43s vs. 0.25s
  system.time(for (j in 1:1000) num_leq_sorted_R(a, b))
  system.time(for (j in 1:1000) num_leq_sorted(a, b))
  
  # Profile C implementation
  # -) ~63% of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:1e4) num_leq_sorted(a, b)
  Rprof(NULL)
  summaryRprof()
}
