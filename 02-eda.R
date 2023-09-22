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
  slice_head(n = 10)

print(top10longest)

#2 ------------------------------------------------------------------
max_val_int <- collatz_df %>%
  filter(max_val == max(max_val)) %>%
  select(start, max_val)

print(max_val_int)

#3 ------------------------------------------------------------------

# For even 

summary_stats_even <- collatz_df %>%
  mutate(is_even = start %% 2 == 0) %>%
  group_by(is_even) %>%
  summarise(
    avg_length = mean(length),
    sd_length = sd(length)
  )
print(summary_stats_even)

# For odd 

summary_stats_odd <- collatz_df %>%
  filter(start %% 2 == 1) %>%
  summarise(
    avg_length_odd = mean(length),
    sd_length_odd = sd(length)
  )
print(summary_stats_odd)

