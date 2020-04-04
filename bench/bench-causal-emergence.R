library(einet)
library(bench)

bench::mark(
  seq(2),

  check = FALSE
)
