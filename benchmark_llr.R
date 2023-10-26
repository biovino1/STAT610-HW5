library(microbenchmark)
source("llr_functions.R")

n = 15
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)

benchmark_result <- microbenchmark(llr(x, y, z, 1))
print(benchmark_result)
