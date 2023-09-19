library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)
library(testthat, quietly = TRUE)

# Task 1 ----------------------------------------------------------------------
gen_collatz <- function(n) {
  if (!is.numeric(n) || n <= 0 || !is.integer(n)) {
    stop("Input must be a positive integer.")
  }
  collatz_seq <- c(n)
  while (n != 1) { 
    if (n %% 2 == 0) {
      n <- n / 2
    } else {
      n <- 3 * n + 1
    }
    collatz_seq <- c(collatz_seq, n)
  }
  return(collatz_seq)
}

collatz_df <- tibble(start = 1:10000, seq = list(rep(NA_real_, 1)),
                     length = NA_real_, parity = NA_character_, 
                     max_val = NA_real_)
for(i in 1:10000) {
  collatz_seq <- gen_collatz(i)
  collatz_df[i, c("seq", "length", "parity", "max_val")] <- list(
    list(collatz_seq), 
    length(collatz_seq),
    ifelse(i %% 2 == 0, "Even", "Odd"),
    max(collatz_seq)
  )
}
head(collatz_df)
print(collatz_df)

