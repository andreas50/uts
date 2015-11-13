#############################################
# Speed analysis of R vs. C implementations #
#############################################

# Hardware: i7-2600, 32GB RAM
# Software: Windows 7 Pro 64bit, R 3.2.0, gcc-4.6.3

### num_leq_sorted
# Conclusion: for long input vectors, the C implementation is around 14 times faster
if (0) {
  set.seed(1)
  a <- sort(runif(1e4))
  b <- sort(runif(1e4))
  
  # R vs. C: 4.10s vs. 0.28s
  system.time(for (j in 1:1000) num_leq_sorted_R(a, b))
  system.time(for (j in 1:1000) num_leq_sorted(a, b))
  
  # Profile C implementation
  # -) ~2/3s of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:1e4) num_leq_sorted(a, b)
  Rprof(NULL)
  summaryRprof()
}


### sorted_union
# Conclusion: for long input vectors, the C implementation is only 2.5 times faster
# BUT: the R implementation a) does less error checking, b) has no numerical tolerance support
if (0) {
  set.seed(1)
  a <- sort(runif(1e4))
  b <- sort(runif(1e4))
  
  # R vs. C: 2.90s vs. 1.18s
  system.time(for (j in 1:2000) sorted_union_R(a, b))
  system.time(for (j in 1:2000) sorted_union(a, b))
  
  # Profile C implementation
  # -) ~30% of time spent in C code
  Rprof(interval=0.01)
  for (j in 1:1e4) sorted_union(a, b)
  Rprof(NULL)
  summaryRprof()
  
  # Profile R implementation
  # -) ~1/2 of time spent in sort()
  Rprof(interval=0.01)
  for (j in 1:5000) sorted_union_R(a, b)
  Rprof(NULL)
  summaryRprof()
}
