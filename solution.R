library(tibble)
library(tidyverse)
# Task 1 ----------------------------------------------------------------------
gen_collatz <- function(n) {
  if (n <= 0 || !is.integer(n)) {
    stop("Input must be a positive integer.")
  }
  collatz_seq <- c(n)
  while (n != 1) { 
    if (n %% 2 == 0) {
      n <- n / 2
    } 
    else {
      n <- 3 * n + 1
    }
    collatz_seq <- c(collatz_seq, n)
  }
  return(collatz_seq)
}
