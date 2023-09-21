# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)
library(testthat, quietly = TRUE)
library(purrr)
# Task 1 ------------------------------------------------------------------
gen_collatz <- function(n) {
  if (n <= 0 || !is.integer(n)) {
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

collatz_df <- tibble(start = integer(), seq = list(), length = numeric(), 
                     parity = character(), max_val = numeric())

for (i in 1:10000) {
  collatz_seq <- gen_collatz(i)
  collatz_df <- add_row(collatz_df, start = i, seq = list(collatz_seq),
                        length = length(collatz_seq), 
                        parity = ifelse(i %% 2 == 0, "Even", "Odd"), 
                        max_val = max(collatz_seq))
}
head(collatz_df)
print(collatz_df)


# Task 2 ------------------------------------------------------------

#1 ------------------------------------------------------------------
collatz_df_fixed <- collatz_df %>%
  mutate(
    seq = map(seq, as.numeric),
    length = as.numeric(length),
    parity = as.character(parity),
    max_val = as.numeric(max_val)
  )

top10longest <- collatz_df_fixed %>%
  arrange(desc(length)) %>%
  head(10)

expect_equal(nrow(top10longest), 10L)
expect_is(collatz_df_fixed$seq, "list")
expect_is(collatz_df_fixed$length, "numeric")
expect_is(collatz_df_fixed$parity, "character")
expect_is(collatz_df_fixed$max_val, "numeric")

print(top10longest)

