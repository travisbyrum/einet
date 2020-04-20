library(einet)
library(bench)


bench::mark(
  effective_information(karate),

  check = FALSE
)
