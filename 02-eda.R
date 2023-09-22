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

top10longest <- collatz_df %>%
  arrange(desc(length)) %>%
  head(10) %>%
  select(start)

print(top10longest)

#2 ------------------------------------------------------------------
max_val_int <- collatz_df %>%
  filter(max_val == max(max_val)) %>%
  select(start)

max_val_int <- max_val_int$start

max_val_row <- collatz_df %>%
  filter(max_val == max(max_val))

highest_max_val <- max_val_row$max_val

cat("starting integer with the highest maximum value:", max_val_int, "\n")
cat("highest maximum value:", highest_max_val, "\n")

print(max_val_int)
print(highest_max_val)

#3 ------------------------------------------------------------------

even_odd_stats <- collatz_df %>%
  group_by(parity) %>%
  summarise(
    even_odd_avg_len = mean(length),
    even_odd_sd_len = sd(length)
  )

print(even_odd_stats)
